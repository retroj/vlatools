#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import chicken scheme)

(use srfi-1
     srfi-13
     args
     doodle
     extras
     matchable)

(define the-file (make-parameter #f))

(define draw-vectors
  (let ((last-x 0)
        (lasy-y 0))
    (lambda (cmd x y z intensity)
      (let ((x (* (+ .5 x) 1000))
            (y (* (+ .5 y) 1000)))
        (case cmd
          ((P) (set! last-x x) (set! last-y y))
          ((L)
           (draw-line last-x last-y x y color: solid-white)
           (set! last-x x) (set! last-y y)))))))

(define (draw-hello-world)
  (for-each
   (lambda (line)
     (cond
      ((string-prefix? ";" line) #f)
      ((string-prefix-ci? "set" line) #f)
      (else
       (with-input-from-string line
         (lambda () (draw-vectors (read) (read) (read) (read) (read)))))))
   (read-lines (the-file))))

(world-inits
 (lambda ()
   (clear-screen)
   (draw-hello-world)))

(world-changes
 (lambda (events dt exit)
   (for-each
    (lambda (e)
      (match e
       (('key 'pressed #\esc)
        (exit #t))
       (('key 'pressed #\q)
        (exit #t))
       (else (void))))
    events)))

(define (terminate message #!optional (result 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (print message)
      (exit result))))

(define opts
  (list
   #;(args:make-option (fit) (required: "GEOMETRY")
                     "fit svg paths into GEOMETRY WxH[+X+Y]"
     (set-fit-geometry! arg))
   ))

(receive (options operands)
    (args:parse (command-line-arguments) opts)
  (when (null? operands)
    (terminate "Please supply a filename"))
  (when (> (length operands) 1)
    (terminate "Too many operands"))
  (the-file (first operands))
  (new-doodle width: 640 height: 480 title: "vlaview" background: solid-black)
  (run-event-loop))
