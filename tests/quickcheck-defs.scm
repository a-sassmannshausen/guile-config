;; quickcheck-defs.scm --- quickcheck definitions    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 01 January 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Quickcheck is a property testing framework originally developed
;; for Haskell. Glean uses the portable scheme implementation of
;; Quickcheck created by IJP.
;;
;; This library defines generators specific to glean data-types
;; for use by quickcheck tests.
;;
;;; Code:

(define-module (tests quickcheck-defs)
  #:use-module (config api)
  #:use-module (quickcheck quickcheck)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-27)
  #:use-module (srfi srfi-64)
  #:export (
            quickcheck-assert
            $short-list
            $configuration
            $secret $switch $setting $argument
            ))

(define (quickcheck-assert name proc . number/generators)
  (test-assert name (apply quickcheck proc number/generators)))

(define ($config-name)
  (let* ((chars (char-set->list
                 (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))
         (lngth (length chars)))
    (string->symbol
     (list->string
      (($short-list
        (lambda _
          (list-ref chars (random-integer lngth)))
        #:non-zero? #t))))))

(define ($safe-symbol)
  (string->symbol ($safe-string)))

(define ($safe-string)
  (let* ((chars (map integer->char (map (lambda (n) (+ 50 n)) (iota 50))))
         (lngth (length chars)))
    (list->string
     (($short-list
       (lambda _ (list-ref chars (random-integer lngth)))
       #:non-zero? #t)))))

(define ($secret)
  (secret
   (name         ($config-name))
   (default      ($safe-string))
   (synopsis     ($safe-string))))

(define ($switch)
  (switch
   (name         ($config-name))
   (default      ($safe-string))
   (test         identity)
   (handler      identity)
   (character    ($char))
   (synopsis     ($safe-string))
   (description  ($safe-string))
   (example      ($safe-string))
   (optional?    ($boolean))))

(define ($setting)
  (setting
   (name         ($config-name))
   (default      ($safe-string))
   (test         identity)
   (handler      identity)
   (character    ($char))
   (synopsis     ($safe-string))
   (description  ($safe-string))
   (example      ($safe-string))
   (optional?    ($boolean))))

(define ($argument)
  (argument
   (name         ($config-name))
   (default      ($safe-string))
   (test         identity)
   (handler      identity) 
   (synopsis     ($safe-string))
   (description  ($safe-string))
   (example      ($safe-string))
   (optional?    ($boolean))))



;; XXX: Some fields to be fixed
(define* ($configuration #:optional (depth 0) #:key random-depth?
                         (keywords (list $secret))
                         (directories (path (given
                                             (string-append
                                              (tmpnam)
                                              file-name-separator-string))
                                            (eager? #t)))
                         (parser identity-parser))
  "Return a randomised <configuration>."
  (lambda ()
    (let ((depth (if random-depth? (random-integer 3) depth)))
      (configuration
       (name ($config-name))
       (synopsis ($safe-string))
       (description ($safe-string))
       (wanted '())                        ; Not testing inheritence atm
       (keywords (($short-list* keywords))) ; Add <switch>? <setting>?
       (arguments (($short-list $argument)))
       (subcommands (if (zero? depth)
                        '()
                        (($short-list ($configuration (1- depth)) 2))))
       (directory directories)
       (version ($safe-string))
       (license #f)  ; To be fixed
       (copyright (($short-list $safe-symbol)))
       (author ($safe-string))
       (parser parser)
       (alias ($safe-symbol))
       (generate-help? ($boolean))
       (generate-usage? ($boolean))
       (generate-version? ($boolean))))))

(define* ($short-list generator #:optional num_problems #:key non-zero?)
  "Return a list with up to ten members of type returned by
GENERATOR."
  (lambda ()
    (build-list (if (number? num_problems) num_problems
                    (if non-zero?
                        (match ($small) (0 1) (n n))
                        ($small)))
                (lambda (_) (generator)))))

(define* ($short-list* generators #:optional num_problems #:key non-zero?)
  "Return a list with up to ten members of type returned by
GENERATOR."
  (lambda ()
    (build-list (if (number? num_problems) num_problems
                    (if non-zero?
                        (match ($small) (0 1) (n n))
                        ($small)))
                (lambda _ ((from-list generators))))))

(define* ($short-assoc key-generator value-generator #:optional guarantee?)
  "Return an association list with up to ten members of type returned by
KEY-GENERATOR and VALUE-GENERATOR.  If GUARANTEE? then ensure the list
contains at least one entry."
  (lambda ()
    (let gen ()
      (let ((lst (($short-list ($pair key-generator value-generator)))))
        (if (and guarantee? (null? lst))
            (gen) lst)))))

(define ($small)
  "Return a integer up to value 10."
  (random-integer 10))

(define (from-list list)
  "Return a random member from LIST."
  (list-ref list (random-integer (length list))))

;; Straight copy from quickcheck.sls
(define (build-list size proc)
  (define (loop i l)
    (if (negative? i)
        l
        (loop (- i 1) (cons (proc i) l))))
  (loop (- size 1) '()))
