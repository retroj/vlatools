#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

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

(use args
     inclub)

(inclub "svgvla-defs.scm")

(define opts
  (list
   (args:make-option (center) optional:
                     "center the drawing at the coordinate system origin"
     (center-drawing #t))
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
  (when (header-file)
    (for-each print (read-lines (header-file))))
  (shapes->vla
   (apply-transformations
    (svg-sxml->shapes
     (read-svg (first operands))))))
