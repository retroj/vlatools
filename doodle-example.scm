
(import chicken scheme)

(use srfi-1
     srfi-13
     extras
     matchable
     doodle)

(define *paint* #f)

(define red '(1 0 0 0.3))

(define last-x 0)
(define last-y 0)

(define (draw-vectors cmd x y intensity . more)
  (let ((y (- y 500)))
    (case cmd
      ((P) (set! last-x x) (set! last-y y))
      ((L)
       (draw-line last-x last-y x y color: solid-black)
       (set! last-x x) (set! last-y y))))
  (unless (null? more)
    (apply draw-vectors more)))

(define (draw-hello-world)
  (let* ((cmds (read-file "myvectors.txt")))
    (apply draw-vectors cmds)))


(world-inits
 (lambda ()
   (clear-screen)
   (set-font! "Vollkorn" 18 red)
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

(new-doodle width: 1000 height: 700 title: "Doodle paint" background: solid-white)
(run-event-loop)
