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
     ssax
     sxpath
     txpath
     utils)


(define (svgpath->vla filename)
  (let* ((svg (with-input-from-file filename
                (lambda () (ssax:xml->sxml (current-input-port) '()))))
         (paths (map
                 (lambda (p)
                   (map
                    (lambda (s) (with-input-from-string s read))
                    (string-tokenize
                     (second p)
                     (char-set-delete char-set:graphic #\,))))
                 ((sxpath '(// http://www.w3.org/2000/svg:path @ d)) svg)))
         (x 0)
         (y 0)
         (mode 'line)
         (curve-coords 0))

    (define (curve-mode?)
      (eq? mode 'curve))

    (define (line-mode?)
      (eq? mode 'line))

    (define translate-path
      (match-lambda*
       (('m dx dy . more)
        (set! x (+ x dx))
        (set! y (+ y dy))
        (set! mode 'line)
        (printf "P ~A ~A 0.0~%" x y)
        (apply translate-path more))
       (('l . more)
        (set! mode 'line)
        (apply translate-path more))
       (('c . more)
        (set! mode 'curve)
        (apply translate-path more))
       ((dx dy . more)
        (cond
         ((line-mode?)
          (set! x (+ x dx))
          (set! y (+ y dy))
          (printf "L ~A ~A 1.0~%" x y))
         ((curve-mode?)
          (set! curve-coords (+ 1 curve-coords))
          (when (= 3 curve-coords)
            (set! curve-coords 0)
            (set! x (+ x dx))
            (set! y (+ y dy))
            (printf "L ~A ~A 1.0~%" x y))))
        (apply translate-path more))
       (_ #t)))

    (for-each
     (lambda (path)
       (set! x 0)
       (set! y 0)
       (set! curve-coords 0)
       (apply translate-path path))
     paths)))

(define opts
  (list))

(receive (options operands)
    (args:parse (command-line-arguments) opts)
  (when (null? operands)
    (abort "Please supply a filename for svg input"))
  (when (> (length operands) 1)
    (abort "Too many operands"))
  (svgpath->vla (first operands)))
