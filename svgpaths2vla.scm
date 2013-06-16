#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import chicken scheme)

(use srfi-1
     srfi-13
     args
     extras
     matchable
     (only ports with-output-to-port)
     regex
     ssax
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

(define (svg-break-path str)
  (map
   read-from-string
   (string-tokenize
    str (char-set-delete char-set:graphic #\,))))

(define svgpath->vla
  (let ((x 0)
        (y 0)
        (mode 'line)
        (curve-coords 0))
    (lambda (path callback)
      (set! x 0)
      (set! y 0)
      (set! curve-coords 0)

      (define (curve-mode?)
        (eq? mode 'curve))

      (define (line-mode?)
        (eq? mode 'line))

      (define the-loop
        (match-lambda*
         (('m dx dy . more)
          (set! x (+ x dx))
          (set! y (+ y dy))
          (set! mode 'line)
          (callback 'position x y)
          (apply the-loop more))
         (('l . more)
          (set! mode 'line)
          (apply the-loop more))
         (('c . more)
          (set! mode 'curve)
          (apply the-loop more))
         ((dx dy . more)
          (cond
           ((line-mode?)
            (set! x (+ x dx))
            (set! y (+ y dy))
            (callback 'line x y))
           ((curve-mode?)
            (set! curve-coords (+ 1 curve-coords))
            (when (= 3 curve-coords)
              (set! curve-coords 0)
              (set! x (+ x dx))
              (set! y (+ y dy))
              (callback 'line x y))))
          (apply the-loop more))
         (_ #t)))

      (apply the-loop path))))

(define (svgpaths->vla svg-sxml)
  (let ((paths (map svg-break-path
                    ((sxpath '(// "svg:path" @ d "text()") xml-namespaces)
                     svg-sxml)))
        (minx #f) (miny #f) (maxx #f) (maxy #f)
        (extents-wid #f) (extents-hei #f))

    (define (extents-calculator _ x y)
      (set! minx (min x (or minx x)))
      (set! miny (min y (or miny y)))
      (set! maxx (max x (or maxx x)))
      (set! maxy (max y (or maxy y))))

    (define (adjust-x x)
      (+ (* (- x (if (fit-ofx) minx 0))
            (if extents-wid (/ (fit-wid) extents-wid) 1))
         (or (fit-ofx) 0)))

    (define (adjust-y y)
      (+ (* (- y (if (fit-ofy) miny 0))
            (if extents-hei (/ (fit-hei) extents-hei) 1))
         (or (fit-ofy) 0)))

    (define (vla-printer type x y)
      (let ((x (adjust-x x))
            (y (adjust-y y)))
        (case type
          ((position) (printf "P ~A ~A 0.0 0.0~%" x y))
          ((line) (printf "L ~A ~A 0.0 1.0~%" x y)))))

    ;; if fit-geometry has been set, then we must calculate the extents of
    ;; the figure.
    (when (fit-wid)
      (for-each
       (lambda (path) (svgpath->vla path extents-calculator))
       paths)
      (set! extents-wid (- maxx minx))
      (set! extents-hei (- maxy miny)))

    (when (header-file)
      (print* (read-all (header-file))))
    (for-each
     (lambda (path) (svgpath->vla path vla-printer))
     paths)))

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
  (svgpaths->vla (read-svg (first operands))))
