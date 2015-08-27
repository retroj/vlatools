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
     linear-algebra
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

(define tau (* 2 pi))

(define rest cdr)

(define (abort-program msg #!optional (status 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (printf "error: ~A~%" msg)
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
      (first
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
            (abort-program (sprintf "failed to parse star designator: ~A" des))))
          (abort-program (sprintf "star designator not found in database: ~A" des)))))
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
  (printf "NAME ~S~%" (asterism-name asterism))
  (printf "IAU ~S~%" (asterism-iau asterism))
  (for-each
   (lambda (path)
     (printf "P ~A~%" (asterism-star-hipparcos asterism (first path)))
     (map (lambda (star)
            (printf "L ~A~%" (asterism-star-hipparcos asterism star)))
          (cdr path)))
   (asterism-paths asterism)))

(define (output-digistar-trail-script asterism)
  (define get-dsob-name
    (let ((i -1))
      (lambda () (sprintf "~Atrail~A" (asterism-iau asterism) (inc! i)))))
  ;; Generate event list
  ;;
  (let* ((objects (asterism-objects asterism))
         (speed 0.5) ;; radians per second
         (start-drawing-time 0.2)
         (trail-dsobs (map (lambda _ (get-dsob-name)) (asterism-paths asterism)))
         (trails-parent-dsob (sprintf "~Atrails" (asterism-iau asterism)))
         (initial-positions
          (map
           (match-lambda
            ((first-object . _)
             (list (asterism-star-ra asterism first-object)
                   (asterism-star-dec asterism first-object))))
           (asterism-paths asterism)))
         (events
          (map
           (match-lambda
            ((dsob time duration ra dec)
             (list dsob (if (< time 0.0)
                            0.0
                            time)
                   duration ra dec)))
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
                             (cons (list trail-dsob time duration ra2 dec2) result))))))
             (asterism-paths asterism)
             trail-dsobs)
            (lambda (a b) (< (second a) (second b)))))))

    (define (trunc n)
      (* 0.001 (truncate (* n 1000))))

    ;; Initialize trails
    ;;
    (let ((first-trail-dsob (first trail-dsobs)))
      (printf "\t~A is trailclass~%" first-trail-dsob)
      (printf "\t~A intensity inherited~%" first-trail-dsob)
      (printf "\t~A frame 1~%" first-trail-dsob)
      (printf "\t~A screenspace off~%" first-trail-dsob)
      (newline)
      (for-each
       (lambda (dsob)
         (printf "\t~A is ~A~%" dsob first-trail-dsob))
       (rest trail-dsobs))
      (newline)
      (for-each
       (match-lambda*
        ((dsob (ra dec))
         (printf "\t~A position celestial ~A ~A 1 ly~%" dsob (trunc ra) (trunc dec))))
       trail-dsobs
       initial-positions)
      (newline)
      (printf "\t~A is emptyclass~%" trails-parent-dsob)
      (printf "\t~A intensity 100~%" trails-parent-dsob)
      (newline)
      (for-each
       (lambda (dsob)
         (printf "\t~A add ~A~%" trails-parent-dsob dsob))
       trail-dsobs)
      (newline)
      (printf "\tscene add ~A~%" trails-parent-dsob)
      (newline))

    ;; Translate event list into Digistar Script
    ;;
    (printf "~A" start-drawing-time)
    (let loop ((event (first events))
               (events (rest events))
               (prevtime 0.0))
      (match event
        ((dsob time duration ra dec)
         (when (> time prevtime)
           (printf "~A" (trunc (+ start-drawing-time time))))
         (printf "\t~A position celestial ~A ~A 1 ly" dsob (trunc ra) (trunc dec))
         (if (> duration 0.0)
             (printf " duration ~A~%" (trunc duration))
             (newline))
         (if (null? events)
             (printf "~A\t## all done~%" (trunc (+ start-drawing-time time duration)))
             (loop (first events) (rest events) time)))))

    (newline)
    (printf "+10\t~A intensity 0 duration 5~%" trails-parent-dsob)
    (printf "+5\t~A delete~%" trails-parent-dsob)
    (for-each
     (lambda (dsob)
       (printf "\t~A delete~%" dsob))
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
       (string-append
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
    (print "usage: make-asterism <input>")
    (exit 1))))
