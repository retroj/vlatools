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

(use (srfi 1 13 14)
     args
     comparse
     data-structures
     fmt
     matchable
     ports)


;;
;; Utils
;;

(define tau (* 4 (asin 1.0)))

(define (degrees->radians d)
  (* tau (/ d 360.0)))

(define (latlon->cartesian3 lat lon)
  (let ((r 6371000))
    (list (* r (cos lat) (cos lon))
          (* r (cos lat) (sin lon))
          (* r (sin lat)))))


;;
;; VLA Output
;;

(define (vlaheader options)
  (let ((title (alist-ref 'title options))
        (author (alist-ref 'author options))
        (site (alist-ref 'site options)))
    (fmt #t
         (if title (cat "set comment " title nl) fmt-null)
         (if author (cat "set author " author nl) fmt-null)
         (if site (cat "set site " site nl) fmt-null)
         "set filetype NEW" nl
         "set coordsys LEFT" nl
         "set defaultdraw STELLAR" nl
         "set filecontent LINES" nl
         "set parametric NON_PARAMETRIC" nl
         "set depthcue " (alist-ref 'vla-depthcue options) nl
         "set library_id UNKNOWN" nl)))


;;
;; Parse Path File
;;

(define jd 2457986.5)

(define (as-number parser)
  (bind (as-string parser)
      (o result string->number)))

(define ws* (zero-or-more (in char-set:blank)))

(define ws+ (one-or-more (in char-set:blank)))

(define eol (in (string->char-set "\n")))

(define whole-number
  (as-number (one-or-more (in char-set:digit))))

(define decimal-number
  (as-number (sequence whole-number (is #\.) whole-number)))

(define time
  (sequence* ((h whole-number)
              (_ (is #\:))
              (m whole-number))
    (result (+ jd (/ (+ h (/ m 60.0)) 24.0)))))

(define latitude-longitude
  (sequence* ((latdeg whole-number)
              (latdec (preceded-by ws+ decimal-number))
              (latsign (in (string->char-set "NS")))
              (londeg (preceded-by ws+ whole-number))
              (londec (preceded-by ws+ decimal-number))
              (lonsign (in (string->char-set "EW"))))
    (result
     (list (degrees->radians (* (if (eqv? latsign #\N) 1 -1)
                                (+ latdeg (/ latdec 60.0))))
           (degrees->radians (* (if (eqv? lonsign #\E) 1 -1)
                                (+ londeg (/ londec 60.0))))))))

(define duration
  (sequence*
   ((m whole-number)
    (_ (is #\m))
    (s decimal-number)
    (_ (is #\s)))
   (result (+ (* 60 m) s))))

(define record
  (sequence* ((_ ws*) (time time)
              (_ ws+) (northern-limit latitude-longitude)
              (_ ws+) (southern-limit latitude-longitude)
              (_ ws+) (central-line latitude-longitude)
              (_ ws+) (m-s-diam-ratio decimal-number)
              (_ ws+) (sun-alt whole-number)
              (_ ws+) (sun-azm whole-number)
              (_ ws+) (path-width-km whole-number)
              (_ ws+) (duration duration))
    (result (list time northern-limit southern-limit
                  central-line m-s-diam-ratio sun-alt
                  sun-azm path-width-km duration))))

(define ignored-line
  (bind (zero-or-more (in (char-set-union char-set:graphic char-set:blank)))
      (lambda _ (result #f))))

(define eclipse-path
  (separated-by (any-of record ignored-line) eol))

(define (parse-file)
  (receive (result remainder)
      (parse eclipse-path (current-input-port))
    (filter identity result)))


;;
;; Main
;;

(define opts
  (list
   (args:make-option
       (h help) #:none
       "help"
     (fmt #t "usage: eclipse-path [options]" nl nl
          (args:usage opts) nl)
     (exit 1))

   (args:make-option
       (o options-file) #:required
       "load additional options alist from file")

   (args:make-option
       (f path-file) #:required
       "data file of eclipse path")

   (args:make-option
       (p path) #:required
       "which path (center*, limits, both)"
     (set! arg (string->symbol (string-downcase arg))))

   (args:make-option
       (vla-depthcue) #:required
       "depthcue value for VLA output (0*, 1, 2)"
     (set! arg (string->number arg)))))


(call-with-values
    (lambda () (args:parse (command-line-arguments) opts))
  (match-lambda*
    ((options ())
     (let* ((options-file (alist-ref 'options-file options))
            (options (append
                      options
                      (if options-file
                          (with-input-from-file options-file read)
                          '())
                      '((path . center)
                        (vla-depthcue . 0))))
            (path-file (alist-ref 'path-file options))
            (path (alist-ref 'path options)))
       (unless path-file
         (fmt #t "Path file must be specified (--path-file)" nl)
         (exit 1))
       (unless (member path '(center limits both))
         (fmt #t "path must be one of center, limits, or both" nl)
         (exit 1))
       (let ((data (filter identity (with-input-from-file path-file parse-file))))
         (vlaheader options)
         (for-each
          (lambda (field)
            (for-each
             (lambda (record draw-command)
               (fmt #t draw-command " "
                    (fmt-join wrt (apply latlon->cartesian3 (field record)) ", ")
                    ", 1.0" nl))
             data
             (cons 'P (circular-list 'L))))
          (alist-ref path `((center ,fourth)
                            (limits ,second ,third)
                            (both ,second ,third ,fourth)))))))
    ((options operands)
     (fmt #t "Too many operands" nl)
     (exit 1))))
