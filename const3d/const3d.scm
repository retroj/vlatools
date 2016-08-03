#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

;; Copyright 2016 John J Foerch. All rights reserved.
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

(use (srfi 1 13)
     args
     data-structures
     extras
     fmt
     matchable)

(define boundaries-filename "bound_in_20.txt")

(define tau 6.283185307179586)

(define title (make-parameter "3D constellation boundary"))
(define constellation-abr (make-parameter #f))
(define author (make-parameter #f))
(define site (make-parameter #f))
(define coordsys (make-parameter 'RIGHT))
(define depthcue (make-parameter 2))

(define (vlaheader-title)
  (if (or (title) (constellation-abr))
      (cat "set comment "
           (fmt-join dsp
                     (filter identity (list (title) (constellation-abr)))
                     " - ")
           nl)
      ""))

(define (vlaheader)
  (fmt #t
       (vlaheader-title)
       "set author " (author) nl
       "set site " (site) nl
       "set filetype NEW" nl
       "set coordsys " (coordsys) nl
       "set defaultdraw STELLAR" nl
       "set filecontent LINES" nl
       "set parametric NON_PARAMETRIC" nl
       "set depthcue " (depthcue) nl
       "set library_id UNKNOWN" nl))

(define-syntax bind-lambda
  (syntax-rules ()
    ((bind-lambda pattern . body)
     (match-lambda (pattern . body)))))

(define-syntax bind-lambda*
  (syntax-rules ()
    ((bind-lambda* pattern . body)
     (match-lambda* (pattern . body)))))

(define (read-boundary constellation)
  (define (parse-line line)
    (with-input-from-string line
      (lambda ()
        (let* ((ra (read))
               (dec (read))
               (const (read)))
          (values ra dec const)))))
  (with-input-from-file boundaries-filename
    (lambda ()
      (let loop ((line (read-line))
                 (lines (list)))
        (cond
         ((eof-object? line) lines)
         (else
          (receive (ra dec const) (parse-line line)
            (cond
             ((eq? const constellation)
              (loop (read-line)
                    (cons (list ra dec) lines)))
             ((null? lines)
              (loop (read-line) lines))
             (else ;; skip rest of file
              lines)))))))))

(define (celestial->cartesian ra dec distance)
  (let ((theta (* tau (/ ra 24.0)))
        (phi (* tau (/ (+ (- dec) 90) 360.0))))
    (if (eq? 'RIGHT (coordsys))
        (values
         (* distance (cos theta) (sin phi))
         (* distance (sin theta) (sin phi))
         (* distance (cos phi)))
        (let ((distance (* 9.4607304725808e15 distance)))
          (values
           (* distance (cos theta) (sin phi))
           (* distance (cos phi))
           (* distance (sin theta) (sin phi)))))))

(define (close-loop coords)
  (append coords (list (first coords))))

(define (main constellation)
  (let ((boundary (close-loop (read-boundary constellation))))
    (vlaheader)
    (let loop-distance ((count 0)
                        (prev-distance #f)
                        (distance 1.0))
      (let loop-boundary ((boundary boundary)
                          (draw-commands (cons 'P (circular-list 'L))))
        (let*-values (((ra dec) (apply values (car boundary)))
                      ((x y z) (celestial->cartesian ra dec distance)))
          (fmt #t (car draw-commands) " " x " " y " " z " 1.0" nl))
        (unless (null? (cdr boundary))
          (loop-boundary (cdr boundary) (cdr draw-commands))))
      (when (< count 9)
        (loop-distance (+ 1 count) distance (* 2 distance))))))


(define opts
  (list
   (args:make-option (c constellation) (#:required "ABR")
                     "hmm"
     (set! arg (string->symbol (string-upcase arg))))))

(receive (options operands)
    (args:parse (command-line-arguments) opts)
  (let ((constellation (alist-ref 'constellation options)))
    (unless constellation
      (fmt #t "Constellation must be specified (-c ABR)" nl)
      (exit 1))
    (author "John Foerch")
    (site "Roger B. Chaffee Planetarium")
    (constellation-abr constellation)
    (coordsys 'RIGHT)
    ;; title
    ;; depthcue
    (main constellation)))
