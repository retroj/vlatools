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
     list-utils
     matchable
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

(define (abort-program msg #!optional (status 1))
  (with-output-to-port (current-error-port)
    (lambda ()
      (printf "error: ~A~%" msg)
      (exit status))))

(define (file-readable? file)
  (and (file-exists? file)
       (regular-file? file)
       (file-read-access? file)))


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
            (hyg-get-records/pattern (string-append "^[^,]+," des ",")))

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

(define (output-digistar-trail-script)
  #t)

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
