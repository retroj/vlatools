#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import chicken scheme)

(use srfi-1
     srfi-13
     args
     extras
     list-utils
     matchable
     (only ports with-output-to-port)
     regex
     ssax
     sxml-transforms
     sxpath
     utils)

(define (read-from-string s)
  (with-input-from-string s read))

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
   (spec (terminate (sprintf "Failed to parse geometry ~S" spec)))
   (else val)))

(define header-file (make-parameter #f))

(define xml-namespaces
  '((svg . "http://www.w3.org/2000/svg")))

(define (read-svg filename)
  (with-input-from-file filename
    (lambda () (ssax:xml->sxml (current-input-port) '()))))

(define (svg-path->points pathspec)
  (define (add-point x y path)
    (cons `(,x ,y) path))
  (filter
   (lambda (x) (not (null? x)))
   (let loop ((x 0)
              (y 0)
              (curpath '())
              (outpaths '())
              (pathspec pathspec))
     (match pathspec
       (() (reverse! (cons curpath outpaths))) ;; done!

       (('M x y . more) ;; absolute moveto
        (loop x y `((,x ,y)) (cons (reverse! curpath) outpaths) more))

       (('m dx dy . more) ;; relative moveto
        (let ((x (+ x dx)) (y (+ y dy)))
          (loop x y `((,x ,y)) (cons (reverse! curpath) outpaths) more)))

       (('L x y . more) ;; absolute lineto
        (loop x y (add-point x y curpath) outpaths more))

       (('l dx dy . more) ;; relative lineto
        (let ((x (+ x dx)) (y (+ y dy)))
          (loop x y (add-point x y curpath) outpaths more)))

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
          (loop x y (add-point x y curpath) outpaths more)))

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

(define (svg-sxml->shapes sxml)
  (let ((shapes '()))
    (define (shape-add! data)
      (set! shapes (cons data shapes)))

    (define svg-rules
      `((*default* . ,(lambda (tag body) body))
        (*text* . ,(lambda (tag body) #f))
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

(define (terminate message #!optional (result 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (print message)
      (exit result))))

(define opts
  (list
   (args:make-option (fit) (required: "GEOMETRY")
                     "fit svg paths into GEOMETRY WxH[+X+Y]"
     (set-fit-geometry! arg))
   (args:make-option (header) (required: "FILE")
                     "include FILE as header in output"
     (header-file arg))))

(receive (options operands)
    (args:parse (command-line-arguments) opts)
  (when (null? operands)
    (terminate "Please supply a filename for svg input"))
  (when (> (length operands) 1)
    (terminate "Too many operands"))
  (shapes->vla
   (svg-sxml->shapes
    (read-svg (first operands)))))
