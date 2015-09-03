
;;; missbehave tests for svgvla

(module svgvla-defs *
  (import chicken scheme)
  (include "svgvla-defs.scm"))

(import svgvla-defs)

(describe "svg-break-path"
  (it "converts a string path spec to a list of symbols and numbers"
      (expect (svg-break-path "M302,193v-54.8l-14.6,9.1v-9.1l21.4-16.9h2.9V193H302z")
              (be '(M 302 193 v -54.8 l -14.6 9.1 v -9.1 l 21.4 -16.9 h 2.9 V 193 H 302 z)))))

(describe "svg-path->points"
  (it "supports relative move command"
      (expect (svg-path->points '(m 200 1000 -5 -25 -2 -50))
              (be '(((200 1000) (195 975) (193 925))))))
  (it "supports absolute move command"
      (expect (svg-path->points '(M 200 1000 -5 -25 -2 -50))
              (be '(((200 1000) (-5 -25) (-2 -50))))))
  (it "treats curve command as a straight line"
      (expect (svg-path->points
               (svg-break-path
                "c 0,0 4,-4 6,-5 2,-1 7,-17 7,-17"))
              (be `(((6 -5) (13 -22)))))))
