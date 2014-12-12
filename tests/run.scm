
(import chicken scheme)

(use inclub
     test)

(inclub "../svgvla-defs.scm")

(test '(M 302 193 v -54.8 l -14.6 9.1 v -9.1 l 21.4 -16.9 h 2.9 V 193 H 302 z)
      (svg-break-path "M302,193v-54.8l-14.6,9.1v-9.1l21.4-16.9h2.9V193H302z"))

(test '(((200 1000) (195 975) (193 925)))
      (svg-path->points '(m 200 1000 -5 -25 -2 -50)))

(test '(((200 1000) (-5 -25) (-2 -50)))
      (svg-path->points '(M 200 1000 -5 -25 -2 -50)))

;;XXX: this test assumes our cheat implementation of the 'c command
(test `(((6 -5) (13 -22)))
      (svg-path->points
       (svg-break-path
        "c 0,0 4,-4 6,-5 2,-1 7,-17 7,-17")))

(test-exit)
