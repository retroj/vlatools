#!/bin/sh
#| -*- scheme -*-
exec csi -s "$0" "$@"
|#

;; Copyright 2014-2015 John J Foerch. All rights reserved.
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
     list-utils
     matchable
     (only miscmacros inc!)
     posix
     regex
     utils)

;; Options
;;

(define hyg-database-field-types
  `((StarID . ,string->number)
    (Hip . ,string->number)
    (HD . ,string->number)
    #;(HR)
    #;(Gliese)
    #;(BayerFlamsteed)
    #;(ProperName)
    (RA . ,string->number)
    (Dec . ,string->number)
    (Distance . ,string->number)
    (Mag . ,string->number)
    (AbsMag . ,string->number)
    #;(Spectrum)
    (ColorIndex . ,string->number)))

(define hyg-database-fields #f)
(define hyg-database-field-converters #f)

(define hyg-database (make-parameter "hygfull.csv"))


;; Utils
;;

(define tau (* 4.0 (asin 1.0)))

(define rest cdr)

(define (abort-program msg #!optional (status 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (fmt #t "error: " msg nl)
      (exit status))))

(define (file-readable? file)
  (and (file-exists? file)
       (regular-file? file)
       (file-read-access? file)))

(define (degrees->radians d)
  (* tau (/ d 360.0)))

(define (hours->radians h)
  (* tau (/ h 24.0)))

(define (angular-separation ra1 dec1 ra2 dec2)
  (let ((ra1 (hours->radians ra1))
        (dec1 (degrees->radians dec1))
        (ra2 (hours->radians ra2))
        (dec2 (degrees->radians dec2)))
    (acos (+ (* (sin dec1) (sin dec2))
             (* (cos dec1) (cos dec2)
                (cos (- ra1 ra2)))))))


;; HYG Database
;;

(define (hyg-get-records/pattern pattern)
  (with-input-from-pipe
   (string-join
    (map qs (list "grep" pattern (hyg-database)))
    " ")
   read-lines))

(define (hyg-get-records designator)
  (zip-alist
   hyg-database-fields
   (map
    (match-lambda ((proc data) (proc data)))
    (zip
     hyg-database-field-converters
     (string-split
      (first ;;XXX: if no lines were found, this gives an error (car)
       (let ((des (->string designator)))
         (or
          (cond
           ;; Hipparcos designator
           ((number? designator)
            ;;XXX: assumes it's the second field
            (hyg-get-records/pattern (string-append "^[^,]\\+," des ",")))

           ;; Bayer designator (AlpHer, Alp2Her)
           ((string-match '(: ($ upper (+ lower))
                              ($ (? numeric))
                              ($ (+ alpha)))
                          des)
            => (lambda (m)
                 (let ((greekletter (list-ref m 1))
                       (superscript (list-ref m 2))
                       (constellation (list-ref m 3)))
                   (hyg-get-records/pattern
                    (string-append ",[0-9]*" greekletter
                                   (if (not (string-null? superscript))
                                       (string-append " *" superscript " *")
                                       "[0-9]\\? *[0-9]\\?")
                                   constellation)))))

           ;; Flamsteed designator (95Her)
           ((string-match '(: ($ (+ numeric))
                              ($ (+ any)))
                          des)
            => (lambda (m)
                 (let ((num (list-ref m 1))
                       (constellation (list-ref m 2)))
                   (hyg-get-records/pattern
                    (string-append "," num "[A-Za-z ]\\+" constellation)))))

           (else
            (abort-program (fmt #f "failed to parse star designator: " des))))
          (abort-program (fmt #f "star designator not found in database: " des)))))
      "," #t)))))

(define (hyg-get-hipparcos/pattern pattern)
  (let ((recs (hyg-get-records/pattern pattern)))
    (and-let* ((first-match (and (not (null? recs)) (first recs)))
               (m (string-match '(: (+ (~ #\,)) #\,
				    ($ (* (~ #\,)))
				    (* any))
				first-match))
	       (hip (list-ref m 1)))
      (string->number hip))))


;; Asterism Record
;;

(define-record-type :asterism
  (%make-asterism name iau paths objects)
  asterism?
  (name asterism-name)
  (iau asterism-iau)
  (paths asterism-paths)
  (objects asterism-objects))

(define (make-asterism spec)
  (let* ((name (first spec))
        (iau (second spec))
        (paths (drop spec 2))
        (objects (map
                  (lambda (designator) (cons designator (hyg-get-records designator)))
                  (delete-duplicates (apply append paths)))))
    (%make-asterism name iau paths objects)))

(define (asterism-star-hipparcos asterism designator)
  (alist-ref
   'Hip
   (alist-ref designator (asterism-objects asterism))))

(define (asterism-star-ra asterism designator)
  (alist-ref
   'RA
   (alist-ref designator (asterism-objects asterism))))

(define (asterism-star-dec asterism designator)
  (alist-ref
   'Dec
   (alist-ref designator (asterism-objects asterism))))


;; Main
;;

(define (output-digistar-asterism asterism)
  (fmt #t "NAME " (wrt (asterism-name asterism)) nl
       "IAU " (wrt (asterism-iau asterism)) nl)
  (for-each
   (lambda (path)
     (fmt #t "P " (asterism-star-hipparcos asterism (first path)) nl)
     (for-each (lambda (star)
                 (fmt #t "L " (asterism-star-hipparcos asterism star) nl))
          (cdr path)))
   (asterism-paths asterism)))

(define (output-digistar-trail-script asterism)
  (define get-dsob-name
    (let ((i -1))
      (lambda () (fmt #f (asterism-iau asterism) "trail" (inc! i)))))
  ;; Generate event list
  ;;
  (let* ((objects (asterism-objects asterism))
         (speed 0.5) ;; radians per second
         (start-drawing-time 0.2)
         (trail-dsobs (map (lambda _ (get-dsob-name)) (asterism-paths asterism)))
         (trails-parent-dsob (fmt #f (asterism-iau asterism) "trails"))
         (initial-positions
          (map
           (match-lambda
            ((first-object . _)
             (list (asterism-star-ra asterism first-object)
                   (asterism-star-dec asterism first-object)
                   first-object)))
           (asterism-paths asterism)))
         (events
          (sort
           (append-map
            (lambda (path trail-dsob)
              (let loop ((time 0.0)
                         (prev (first path))
                         (remainder (rest path))
                         (result '()))
                (if (null? remainder)
                    result
                    (let* ((next (first remainder))
                           (ra1 (asterism-star-ra asterism prev))
                           (dec1 (asterism-star-dec asterism prev))
                           (ra2 (asterism-star-ra asterism next))
                           (dec2 (asterism-star-dec asterism next))
                           (sep (angular-separation ra1 dec1 ra2 dec2))
                           (duration (/ sep speed))
                           (nexttime (+ time duration)))
                      (loop nexttime next (rest remainder)
                            (cons (list trail-dsob time duration ra2 dec2 next) result))))))
            (asterism-paths asterism)
            trail-dsobs)
           (lambda (a b) (< (second a) (second b))))))

    (define (trunc n)
      (num n 10 3))

    ;; Initialize trails
    ;;
    (let ((first-trail-dsob (first trail-dsobs)))
      (fmt #t "\t" first-trail-dsob " is trailclass" nl
           "\t" first-trail-dsob " intensity inherited" nl
           "\t" first-trail-dsob " frame 1" nl
           "\t" first-trail-dsob " screenspace off" nl nl)
      (for-each
       (lambda (dsob)
         (fmt #t "\t" dsob " is " first-trail-dsob nl))
       (rest trail-dsobs))
      (newline)
      (for-each
       (match-lambda*
        ((dsob (ra dec object))
         (fmt #t "\t" dsob " position celestial "
              (trunc ra) " " (trunc dec) " 1 ly ## "
              object nl)))
       trail-dsobs
       initial-positions)
      (fmt #t nl "\t" trails-parent-dsob " is emptyclass" nl
           "\t" trails-parent-dsob " intensity 100" nl nl)
      (for-each
       (lambda (dsob)
         (fmt #t "\t" trails-parent-dsob " add " dsob nl))
       trail-dsobs)
      (fmt #t nl "\tscene add " trails-parent-dsob nl nl))

    ;; Translate event list into Digistar Script
    ;;
    (fmt #t start-drawing-time)
    (let loop ((event (first events))
               (events (rest events))
               (prevtime 0.0))
      (match event
        ((dsob time duration ra dec object)
         (when (> time prevtime)
           (fmt #t (trunc (+ start-drawing-time time))))
         (fmt #t "\t" dsob " position celestial " (trunc ra) " " (trunc dec)
              " 1 ly duration " (trunc duration) " ## " object nl)
         (if (null? events)
             (fmt #t (trunc (+ start-drawing-time time duration)) "\t## all done" nl)
             (loop (first events) (rest events) time)))))

    (fmt #t nl "+10\t" trails-parent-dsob " intensity 0 duration 5" nl
         "+5\t" trails-parent-dsob " delete" nl)
    (for-each
     (lambda (dsob)
       (fmt #t "\t" dsob " delete" nl))
     trail-dsobs)))

(define output-format (make-parameter output-digistar-asterism))

(define command-line-options
  (list
   (args:make-option (hyg-database) (required: "FILENAME")
                     "Set location of HYG Database file"
     (hyg-database arg))

   (args:make-option (trail) #:none
                     "Output a Digistar script that draws a trail"
     (output-format output-digistar-trail-script))))

(call-with-values
    (lambda () (args:parse (command-line-arguments) command-line-options))
  (match-lambda*
   ((options (input))
    (unless (file-readable? input)
      (abort-program "input file not readable"))
    (unless (file-readable? (hyg-database))
      (abort-program
       (fmt #f
        "HYG database (" (hyg-database) ") not found or not readable.\n"
        "  Obtain hygfull.csv from https://github.com/astronexus/HYG-Database/")))
    (let ((def (with-input-from-file input read)))
      (let ((hyg-database-fields-spec
             (with-input-from-file (hyg-database) read-line)))
        (set! hyg-database-fields
              (map string->symbol (string-split hyg-database-fields-spec ",")))
        (set! hyg-database-field-converters
              (map (lambda (field)
                     (or (alist-ref field hyg-database-field-types)
                         identity))
                   hyg-database-fields)))
      ((output-format) (make-asterism def))))
   (_
    (fmt #t "usage: make-asterism <input>" nl)
    (exit 1))))
