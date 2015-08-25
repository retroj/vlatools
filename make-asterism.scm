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
     extras
     matchable
     posix
     regex
     utils)

;; Options
;;

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

(define (hyg-get-records pattern)
  (with-input-from-pipe
   (string-join
    (map qs (list "grep" pattern (hyg-database)))
    " ")
   read-lines))

(define (hyg-get-hipparcos pattern)
  (let ((recs (hyg-get-records pattern)))
    (and-let* ((first-match (and (not (null? recs)) (first recs)))
               (m (string-match '(: (+ (~ #\,)) #\,
				    ($ (* (~ #\,)))
				    (* any))
				first-match))
	       (hip (list-ref m 1)))
      (string->number hip))))

;; star-hipparcos returns the hipparcos designator for the given
;; hipparcos, flamsteed, or bayer star designation.  If the argument is
;; numeric, then it is considered to be a hipparcos designator already and
;; returned unchanged.  Otherwise the argument may be a string or a
;; symbol, and will be parsed as a bayer (AlpHer) or flamsteed (95Her)
;; designation, and looked up in the HYG Database.
;;
(define (star-hipparcos hip/flamsteed/bayer)
  (if (number? hip/flamsteed/bayer)
      ;; Hipparcos designator; return unchanged.
      hip/flamsteed/bayer
      (let ((des (->string hip/flamsteed/bayer)))
        (or
         (cond

          ;; Bayer designator (AlpHer, Alp2Her)
          ((string-match '(: ($ upper (+ lower))
                             ($ (? numeric))
                             ($ (+ alpha)))
                         des)
           => (lambda (m)
                (let ((greekletter (list-ref m 1))
                      (superscript (list-ref m 2))
                      (constellation (list-ref m 3)))
                  (hyg-get-hipparcos
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
                  (hyg-get-hipparcos
                   (string-append "," num "[A-Za-z ]\\+" constellation)))))

          (else
           (abort-program (sprintf "failed to parse star designator: ~A" des))))
         (abort-program (sprintf "star designator not found in database: ~A" des))))))


;; Main
;;

(define-record-type :asterism
  (%make-asterism name iau paths)
  asterism?
  (name asterism-name)
  (iau asterism-iau)
  (paths asterism-paths))

(define (make-asterism spec)
  (let ((name (first spec))
        (iau (second spec))
        (paths (drop spec 2)))
    (%make-asterism name iau paths)))

(define (output-digistar-asterism asterism)
  (printf "NAME ~S~%" (asterism-name asterism))
  (printf "IAU ~S~%" (asterism-iau asterism))
  (for-each
   (lambda (path)
     (printf "P ~A~%" (star-hipparcos (first path)))
     (map (lambda (star)
            (printf "L ~A~%" (star-hipparcos star)))
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
        "HYG database (./hygfull.csv) not found or not readable.\n"
        "  Obtain hygfull.csv from https://github.com/astronexus/HYG-Database/")))
    (let ((def (with-input-from-file input read)))
      ((output-format) (make-asterism def))))
   (_
    (print "usage: make-asterism <input>")
    (exit 1))))
