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
     matchable
     (only miscmacros dotimes))

(define boundaries-filename "data/constellations/bound_in_20.txt")

(define output-formats '(OBJ VLA))

(define tau 6.283185307179586)

(define constellation-abr (make-parameter #f))

(define (vlaheader-title title)
  (define (substitute x)
    (if (eq? 'iau x) (constellation-abr) x))
  (cat "set comment "
       (if (pair? title)
           (apply-cat (map substitute title))
           (substitute title))
       nl))

(define (vlaheader options)
  (let ((title (alist-ref 'title options))
        (author (alist-ref 'author options))
        (site (alist-ref 'site options)))
    (fmt #t
         (if title (vlaheader-title title) fmt-null)
         (if author (cat "set author " author nl) fmt-null)
         (if site (cat "set site " site nl) fmt-null)
         "set filetype NEW" nl
         "set coordsys " (alist-ref 'vla-coordsys options) nl
         "set defaultdraw STELLAR" nl
         "set filecontent LINES" nl
         "set parametric NON_PARAMETRIC" nl
         "set depthcue " (alist-ref 'vla-depthcue options) nl
         "set library_id UNKNOWN" nl)))

(define-syntax bind-lambda
  (syntax-rules ()
    ((bind-lambda pattern . body)
     (match-lambda (pattern . body)))))

(define-syntax bind-lambda*
  (syntax-rules ()
    ((bind-lambda* pattern . body)
     (match-lambda* (pattern . body)))))

(define (celestial->cartesian/left ra dec distance)
  (let ((theta (* tau (/ ra 24.0)))
        (phi (* tau (/ (+ (- dec) 90) 360.0))))
    (list
     (* distance (cos theta) (sin phi))
     (* distance (cos phi))
     (* distance (sin theta) (sin phi)))))

(define (celestial->cartesian/right ra dec distance)
  (let ((theta (* tau (/ ra 24.0)))
        (phi (* tau (/ (+ (- dec) 90) 360.0))))
    (list
     (* distance (cos theta) (sin phi))
     (* distance (sin theta) (sin phi))
     (* distance (cos phi)))))

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
                 (result (list)))
        (cond
         ((eof-object? line) result)
         (else
          (receive (ra dec const) (parse-line line)
            (cond
             ((eq? const constellation)
              (loop (read-line)
                    (cons (list ra dec) result)))
             ((null? result)
              (loop (read-line) result))
             (else ;; skip rest of file
              result)))))))))

(define (close-loop coords)
  (append coords (list (first coords))))

(define (main/obj constellation options)
  (let* ((boundary (read-boundary constellation))
         (nboundary-verts (length boundary))
         (rings 3)
         (max-dist (expt 2.0 (- rings 1)))
         (step (/ max-dist rings))
         (vertices (append-map!
                    (lambda (distance)
                      (map
                       (match-lambda
                         ((ra dec) (celestial->cartesian/right ra dec distance)))
                       boundary))
                    (iota rings 1.0 step))))
    (for-each
     (match-lambda
       ((x y z) (fmt #t "v " x " " y " " z nl)))
     vertices)
    (define (write-face a b c)
      (let ((a (+ 1 a)) ;; vertices are 1-based indexed
            (b (+ 1 b))
            (c (+ 1 c)))
        (fmt #t "f " a " " b " " c nl)))
    (dotimes (ring (- rings 1))
      (let ((first (* ring nboundary-verts)))
        (dotimes (i nboundary-verts)
          (write-face (+ first (modulo (+ 1 i) nboundary-verts))
                      (+ first i)
                      (+ first i nboundary-verts))
          (write-face (+ first (modulo (+ 1 i) nboundary-verts))
                      (+ first (+ i nboundary-verts))
                      (+ first (modulo (+ 1 i) nboundary-verts) nboundary-verts)))))))

(define (main/vla constellation options)
  (let* ((boundary (close-loop (read-boundary constellation)))
         (coordsys (alist-ref 'vla-coordsys options))
         (celestial->cartesian (if (eq? 'left coordsys)
                                   celestial->cartesian/left
                                   celestial->cartesian/right)))
    (vlaheader options)
    (let loop-distance ((count 0)
                        (prev-distance #f)
                        (distance 1.0))
      (let ((distance (if (eq? 'left coordsys)
                          (* 9.4607304725808e15 distance)
                          distance)))
        (for-each
         (match-lambda*
           (((ra dec) draw-command)
            (fmt #t draw-command " "
                 (fmt-join dsp (celestial->cartesian ra dec distance) " ")
                 " 1.0" nl)))
         boundary
         (cons 'P (circular-list 'L))))
      (when (< count 9)
        (loop-distance (+ 1 count) distance (* 2 distance))))))

(define opts
  (list
   (args:make-option (h help) #:none
                     "help"
     (fmt #t "usage: const3d [options] <const>" nl nl
          " const: IAU abbreviation of a constellation (ori)" nl nl
          (args:usage opts) nl)
     (exit 1))

   (args:make-option (f format) #:required
                     (string-append "output format ("
                                    (fmt #f (fmt-join dsp output-formats ", "))
                                    ")")
     (let ((format (string->symbol (string-upcase arg))))
       (unless (memq format output-formats)
         (fmt #t "Unsupported output format: " format nl)
         (exit 1))
       (set! arg format)))

   (args:make-option (o options-file) #:required
                     "load additional options alist from file")))

(call-with-values
    (lambda () (args:parse (command-line-arguments) opts))
  (match-lambda*
    ((options ())
     (fmt #t "Constellation must be specified" nl)
     (exit 1))
    ((options (constellation))
     (let* ((constellation (string->symbol (string-upcase constellation)))
            (options-file (alist-ref 'options-file options))
            (options (append
                      options
                      (if options-file
                         (append options
                                 (with-input-from-file options-file read))
                         '())
                      '((title . ("3D constellation boundary - " iau))
                        (vla-coordsys . right)
                        (vla-depthcue . 2)))))
       (constellation-abr constellation)
       (case (or (alist-ref 'format options) 'VLA)
         ((VLA) (main/vla constellation options))
         ((OBJ) (main/obj constellation options)))))
    ((options (constellation . rest))
     (fmt #t "Too many operands" nl)
     (exit 1))))
