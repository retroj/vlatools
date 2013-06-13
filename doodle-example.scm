#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import chicken scheme)

(use srfi-1
     srfi-13
     extras
     matchable
     doodle)

(define last-x 0)
(define last-y 0)

(define (draw-vectors cmd x y intensity)
  (let ((x (* (+ 10 x) 10))
        (y (* (+ 10 y) 10)))
    (case cmd
      ((P) (set! last-x x) (set! last-y y))
      ((L)
       (draw-line last-x last-y x y color: solid-black)
       (set! last-x x) (set! last-y y)))))

(define (draw-hello-world)
  (for-each
   (lambda (line)
     (cond
      ((string-prefix-ci? "set" line) #f)
      (else
       (with-input-from-string line
         (lambda () (draw-vectors (read) (read) (read) (read)))))))
   (read-lines "foo.vla")))

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
       (else (void))))
    events)))

(new-doodle width: 640 height: 480 title: "Doodle paint" background: solid-white)
(run-event-loop)
