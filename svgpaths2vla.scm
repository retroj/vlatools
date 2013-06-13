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
     ssax
     sxpath
     txpath
     utils)

(define header-file (make-parameter #f))

(define xml-namespaces
  '((svg . "http://www.w3.org/2000/svg")))

(define (read-svg filename)
  (with-input-from-file filename
    (lambda () (ssax:xml->sxml (current-input-port) '()))))

(define (svg-break-path str)
  (map
   (lambda (s) (with-input-from-string s read))
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
          (callback 'point x y)
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

(define (vla-printer type x y)
  (case type
   ((point) (printf "P ~A ~A 0.0~%" x y))
   ((line) (printf "L ~A ~A 1.0~%" x y))))

(define (svgpaths->vla svg-sxml)
  (when (header-file)
    (print* (read-all (header-file))))
  (for-each
   (lambda (path) (svgpath->vla path vla-printer))
   (map svg-break-path
        ((sxpath '(// "svg:path" @ d "text()") xml-namespaces)
         svg-sxml))))

(define (terminate message #!optional (result 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (print message)
      (exit result))))

(define opts
  (list
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
