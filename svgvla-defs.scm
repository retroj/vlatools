
;; Copyright 2013-2014 John J Foerch. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(import chicken scheme)

(use srfi-1
     srfi-13
     data-structures
     extras
     list-utils
     matchable
     ports
     regex
     ssax
     sxml-transforms
     utils)

(define (read-from-string s)
  (with-input-from-string s read))

(define center-drawing (make-parameter #f))

(define (parse-geometry spec)
  (and-let* ((m (string-match
                 '(: ($ (+ num)) "x" ($ (+ num))
                     (? ($ ("+-") (+ num)) ($ ("+-") (+ num))))
                 spec)))
    (map (lambda (s) (if s (read-from-string s) s))
         (cdr m))))

(define fit-wid (make-parameter #f))
(define fit-hei (make-parameter #f))
(define fit-ofx (make-parameter #f))
(define fit-ofy (make-parameter #f))

(define (set-fit-geometry! spec)
  (cond
   ((and spec (parse-geometry spec))
    => (lambda (g)
         (fit-wid (first g))
         (fit-hei (second g))
         (fit-ofx (third g))
         (fit-ofy (fourth g))))
   (else (terminate (sprintf "Failed to parse geometry ~S" spec)))))

(define header-file (make-parameter #f))

(define xml-namespaces
  '((svg . "http://www.w3.org/2000/svg")))

(define (read-svg filename)
  (with-input-from-file filename
    (lambda () (ssax:xml->sxml (current-input-port) '()))))

(define (svg-path->points pathspec)
  (define (add-point x y path)
    (cons `(,x ,y) path))
  (define (apply-default-command pathspec default-command)
    (if (or (null? pathspec) (symbol? (car pathspec)))
        pathspec
        (cons default-command pathspec)))
  (filter
   (lambda (x) (not (null? x)))
   (let loop ((x 0)
              (y 0)
              (curpath '())
              (outpaths '())
              (pathspec pathspec))
     (match pathspec
       (() (reverse! (cons (reverse! curpath) outpaths))) ;; done!

       (('M . more) ;; absolute moveto
        (loop x y `() (cons (reverse! curpath) outpaths)
              (apply-default-command more 'L)))

       (('m . more) ;; relative moveto
        (loop x y `() (cons (reverse! curpath) outpaths)
              (apply-default-command more 'l)))

       (('L x y . more) ;; absolute lineto
        (loop x y (add-point x y curpath) outpaths
              (apply-default-command more 'L)))

       (('l dx dy . more) ;; relative lineto
        (let ((x (+ x dx)) (y (+ y dy)))
          (loop x y (add-point x y curpath) outpaths
                (apply-default-command more 'l))))

       (('H x . more) ;; absolute horizontal lineto
        (loop x y (add-point x y curpath) outpaths more))

       (('h dx . more) ;; relative horizontal lineto
        (let ((x (+ x dx)))
          (loop x y (add-point x y curpath) outpaths more)))

       (('V y . more) ;; absolute vertical lineto
        (loop x y (add-point x y curpath) outpaths more))

       (('v dy . more) ;; relative vertical lineto
        (let ((y (+ y dy)))
          (loop x y (add-point x y curpath) outpaths more)))

       (('c x1 y1 x2 y2 dx dy . more) ;; relative curveto
        ;; cheat and just do a line to dx,dy
        (let ((x (+ x dx)) (y (+ y dy)))
          (loop x y (add-point x y curpath) outpaths
                (apply-default-command more 'c))))

       (('s x2 y2 dx dy . more) ;; shorthand relative curveto
        ;; cheat and just do a line to dx,dy
        (let ((x (+ x dx)) (y (+ y dy)))
          (loop x y (add-point x y curpath) outpaths
                (apply-default-command more 's))))

       (('Z . more) ;; close subpath
        (let* ((pt (last curpath))
               (x (first pt))
               (y (second pt)))
          (loop x y (add-point x y curpath) outpaths more)))

       (('z . more) ;; close subpath
        (let* ((pt (last curpath))
               (x (first pt))
               (y (second pt)))
          (loop x y (add-point x y curpath) outpaths more)))

       (more
        (terminate (sprintf "error: svg-path->points failed to parse:~%  ~S~%" more)))))))

(define (svg-break-path str)
  (map read-from-string
       (string-split-fields "[a-zA-Z]|(-?[.0-9]+)" str)))

(define (svg-break-polygon-points str)
  (section
   (map read-from-string (string-split str " \n\r\t,"))
   2))

(define (svg-rect->points x y width height)
  (list (list x y)
        (list (+ x width) y)
        (list (+ x width) (+ y height))
        (list x (+ y height))
        (list x y)))

(define (svg-sxml->shapes sxml)
  (let ((shapes '()))
    (define (shape-add! data)
      (set! shapes (cons data shapes)))

    (define svg-rules
      `((*default* . ,(lambda (tag body) body))
        (*text* . ,(lambda (tag body) #f))
        (http://www.w3.org/2000/svg:line
         *preorder* . ,(match-lambda*
                        ((tag (('@ . attrs) . body))
                         (and-let* ((x1 (string->number (cadr (assoc 'x1 attrs))))
                                    (x2 (string->number (cadr (assoc 'x2 attrs))))
                                    (y1 (string->number (cadr (assoc 'y1 attrs))))
                                    (y2 (string->number (cadr (assoc 'y2 attrs)))))
                           (shape-add! `((,x1 ,y1) (,x2 ,y2)))))
                        (x #f)))
        (http://www.w3.org/2000/svg:path
         *preorder* . ,(match-lambda*
                        ((tag (('@ . attrs) . body))
                         (and-let* ((r (assoc 'd attrs)))
                           (for-each
                            shape-add!
                            (svg-path->points
                             (svg-break-path (cadr r))))))
                        (x #f)))
        (http://www.w3.org/2000/svg:polygon
         *preorder* . ,(match-lambda*
                        ((tag (('@ . attrs) . body))
                         (and-let* ((r (assoc 'points attrs))
                                    (points (svg-break-polygon-points (cadr r))))
                           (shape-add! (cons (last points) points))))
                        (x #f)))
        (http://www.w3.org/2000/svg:polyline
         *preorder* . ,(match-lambda*
                        ((tag (('@ . attrs) . body))
                         (and-let* ((r (assoc 'points attrs))
                                    (points (svg-break-polygon-points (cadr r))))
                           (shape-add! points)))
                        (x #f)))
        (http://www.w3.org/2000/svg:rect
         *preorder* . ,(match-lambda*
                        ((tag (('@ . attrs) . body))
                         (and-let* ((x (assoc 'x attrs))
                                    (y (assoc 'y attrs))
                                    (width (assoc 'width attrs))
                                    (height (assoc 'height attrs)))
                           (shape-add! (apply svg-rect->points
                                              (map (compose string->number cadr)
                                                   (list x y width height))))))
                        (x #f)))))

    (define (find-shapes sxml)
      (pre-post-order* sxml svg-rules) ;;traverse tree for side-effect, throw away result
      (set! shapes (reverse! shapes))
      shapes)

    (find-shapes sxml)))

(define (shapes->vla shapes)
  (define print-point
    (match-lambda*
     (((x y) command)
      (printf "~A ~A ~A 0.0 1.0~%" command x y))))
  (for-each
   (lambda (shape)
     (print-point (car shape) 'P)
     (for-each
      (lambda (point) (print-point point 'L))
      (cdr shape)))
   shapes))

(define (apply-transformations shapes)
  (match-let
      (((xmin xmax ymin ymax)
        (fold
         (match-lambda*
          ((shape (xmin xmax ymin ymax))
           (receive (xs ys)
               (unzip-alist shape)
             (let ((ys (map car ys)))
               (list (apply min xmin xs)
                     (apply max xmax xs)
                     (apply min ymin ys)
                     (apply max ymax ys))))))
         (match (caar shapes) ((x y) (list x x y y)))
         shapes)))
    (let ((transformations
           (list
            (list (center-drawing)
                  (lambda (shapes)
                    (let ((hoff (* 0.5 (+ xmin xmax)))
                          (voff (* 0.5 (+ ymin ymax))))
                      (map (lambda (shape)
                             (map (match-lambda ((x y) (list (- x hoff) (- y voff))))
                                  shape))
                           shapes)))))))
      (fold
       (match-lambda*
        (((test consequent) shapes)
         (if test
             (consequent shapes)
             shapes)))
       shapes
       transformations))))

(define (terminate message #!optional (result 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (print message)
      (exit result))))
