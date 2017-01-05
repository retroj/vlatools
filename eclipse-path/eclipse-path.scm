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
    (list (* r (cos lon) (cos lat))
          (* r (sin lon) (cos lat))
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

(define (as-number parser)
  (bind (as-string parser)
      (o result string->number)))

(define blank* (zero-or-more (in char-set:blank)))

(define blank+ (one-or-more (in char-set:blank)))

(define eol (in (string->char-set "\n")))

(define whole-number
  (as-number (one-or-more (in char-set:digit))))

(define decimal-number
  (as-number (sequence whole-number (is #\.) whole-number)))

(define time
  (sequence* ((h whole-number)
              (_ (is #\:))
              (m whole-number))
    (result (/ (+ h (/ m 60.0)) 24.0))))

(define latitude-longitude
  (sequence* ((latdeg whole-number)
              (latdec (preceded-by blank+ decimal-number))
              (latsign (in (string->char-set "NS")))
              (londeg (preceded-by blank+ whole-number))
              (londec (preceded-by blank+ decimal-number))
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

(define (record jd)
  (sequence* ((_ blank*) (time time)
              (_ blank+) (northern-limit latitude-longitude)
              (_ blank+) (southern-limit latitude-longitude)
              (_ blank+) (central-line latitude-longitude)
              (_ blank+) (m-s-diam-ratio decimal-number)
              (_ blank+) (sun-alt whole-number)
              (_ blank+) (sun-azm whole-number)
              (_ blank+) (path-width-km whole-number)
              (_ blank+) (duration duration))
    (result (list (+ jd time) northern-limit southern-limit
                  central-line m-s-diam-ratio sun-alt
                  sun-azm path-width-km duration))))


;; we need a parser and an interface to get values by name from parsed
;; results.
'(define-record-type :ad-hoc-text-catalog
  (make-ad-hoc-text-catalog parser ref)
  ad-hoc-text-catalog?
  (parser ad-hoc-text-catalog-parser)
  (ref ad-hoc-text-catalog-ref))

'(ad-hoc-text
  leading: blank*
  separator: blank+
  strict: #f
  fields: `((time ,time)
            (northern-limit ,latitude-longitude)
            (southern-limit ,latitude-longitude)
            (central-line ,latitude-longitude)
            (m-s-diam-ratio ,decimal-number)
            (sun-alt ,whole-number)
            (sun-azm ,whole-number)
            (path-width-km ,whole-number)
            (duration ,duration)))

;; so given this ad-hoc catalog as defined above, what do the options look
;; like that use it?  It's pretty clear that catalogs need to be defined
;; as modules in some kind of plugin system, if we don't want to introduce
;; evaluation into the options system.  Unless we allow all of these
;; parsers to be named by symbol, which could work.  The ad hoc catalog
;; module would provide the parsers that it knows about.
;;
;; how general is this ad-hoc catalog system, really?
;;
;; how do we know that this catalog defines a series of events pertaining
;; to a single object?
;;


(define ignored-line
  (bind (zero-or-more (in (char-set-union char-set:graphic char-set:blank)))
      (lambda _ (result #f))))

(define (eclipse-path jd)
  (separated-by (any-of (record jd) ignored-line) eol))

(define (load-catalog options)
  (let ((path-file (alist-ref 'path-file options)))
    (filter
     identity
     (with-input-from-file path-file
       (lambda ()
         (receive (result remainder)
             (parse (eclipse-path 2457986.5) (current-input-port))
           (filter identity result)))))))


;;
;; Output
;;

(define (output-vla options data)
  (let ((path (alist-ref 'path options)))
    (vlaheader options)
    (for-each
     (lambda (field)
       (for-each
        (lambda (record draw-command)
          (match-let (((x y z) (apply latlon->cartesian3 (field record))))
            (fmt #t draw-command " " (fmt-join wrt (list x z y 1.0) ", ") nl)))
        data
        (cons 'P (circular-list 'L))))
     (alist-ref path `((center ,fourth)
                       (limits ,second ,third)
                       (both ,second ,third ,fourth))))))

(define (output-dspath options data)
  (let ((path (alist-ref 'path options)))
    (fmt #t "Ds," (or (alist-ref 'title options) "Eclipse Path")
         ",Earth-Centered,,,,," nl
         "Face,FALSE,(TRUE or FALSE),,,,," nl
         "Date,TRUE,(TRUE or FALSE),,,,," nl
         "XYZHPR,0x38,(BITMASK),,,,," nl
         "Segments,SMOOTH,(SMOOTH or STRAIGHT),,,,," nl
         ",Time,X,Y,Z,H,P,R" nl)
    (for-each
     (lambda (record)
       (fmt #t "Node Data," (first record) ","
            (fmt-join wrt (apply latlon->cartesian3 (fourth record)) ",")
            ",0,0,0" nl))
     data)))


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
       (output) #:required
       "output format (vla*, dspath)"
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
                      '((output . vla)
                        (path . center)
                        (vla-depthcue . 0))))
            (output (alist-ref 'output options))
            (path-file (alist-ref 'path-file options))
            (path (alist-ref 'path options)))
       (unless path-file
         (fmt #t "Path file must be specified (--path-file)" nl)
         (exit 1))
       (unless (member path '(center limits both))
         (fmt #t "path must be one of center, limits, or both" nl)
         (exit 1))
       (let ((data (load-catalog options)))
         (case output
           ((vla) (output-vla options data))
           ((dspath) (output-dspath options data))))))
    ((options operands)
     (fmt #t "Too many operands" nl)
     (exit 1))))
