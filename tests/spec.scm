;; spec.scm --- tests for spec    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2015 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 04 December 2015
;;
;; This file is part of Config.
;;
;; Config is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; Config is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with config; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Unit tests for spec.scm.
;;
;; Source-file: conf/spec.scm
;;
;;; Code:

(define-module (tests spec)
  #:use-module (conf)
  #:use-module (conf spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  )

(define* (error-catcher proc #:optional (key 'config-spec))
  "Return a procedure which returns #t if PROC applied to its args raises an
error of type 'config-spec.

This is used to test that we raise errors correctly."
  (lambda (args)
           (catch key
             ;; Kludgy way of not needing to specify list when using
             ;; `error-catcher'.  This will fail when proc is arity 1 and
             ;; expects a list.
             (lambda () (if (list? args) (apply proc args) (proc args)) #f)
             (lambda (k a) #t))))


;;;; Tests

(test-begin "spec")

;;;;; Tests for: private-option

(test-assert "prioption-creation"
  (every (compose private-option? (cut apply private-option <>))
         `((test "Test terse")
           (test "Test terse" #:test ,identity)
           (test "Test terse" #:value 5)
           (test "Terse" #:long "Test documentation.")
           (test "Terse" #:test ,(const 5) #:long "Blah")
           (test "T" #:value #\x #:test ,+ #:long "L"))))

(test-assert "prioption-fail"
  (every (error-catcher private-option)
         '(("test" "Test terse")               ; Faulty name
           (test "Test terse" #:test identity) ; Faulty test
           (test Test #:value 5)               ; Faulty terse
           (test "Terse" #:long Test))))       ; Faulty long

;;;;; Tests for: public-option

(test-assert "puboption-creation"
  (every (compose public-option? (cut apply public-option <>))
        `((test "Test terse")
          (test "Terse" #:long "Test documentation.")
          (test "Test terse" #:single-char #\t)
          (test "Test terse" #:handler ,string->symbol)
          (test "Test terse" #:example "Example")
          (test "Test terse" #:optional? #t))))

(test-assert "puboption-fail"
  (every (error-catcher public-option)
         '(("test" "Test terse")                         ; Faulty name
           (test Test)                                   ; Faulty terse
           (test "Terse" #:long Test)                    ; Faulty long
           (test "Terse" #:single-char 't)               ; Faulty single-char
           (test "Test terse" #:handler 'string->symbol) ; Faulty handler
           (test "Terse" #:example 'test)                ; Faulty example
           (test "Terse" #:optional? "hello"))))         ; Faulty optional

;;;;; Tests for: open-option

(test-assert "openoption-creation"
  (every (compose open-option? (cut apply open-option <>))
        `((test "Test terse")
          (test "Test terse" #:single-char #\t)
          (test "Test terse" #:value #\t)
          (test "Test terse" #:test ,identity)
          (test "Test terse" #:handler ,string->symbol)
          (test "Terse" #:long "Test documentation.")
          (test "Test terse" #:example "Example")
          (test "Test terse" #:optional? #t))))

(test-assert "openoption-fail"
  (every (error-catcher open-option)
         '(("test" "Test terse")                         ; Faulty name
           (test Test)                                   ; Faulty terse
           (test "Terse" #:single-char 't)               ; Faulty single-char
           (test "Test terse" #:test quoted)             ; Faulty test
           (test "Test terse" #:handler quoted)          ; Faulty handler
           (test "Terse" #:long Test)                    ; Faulty long
           (test "Terse" #:example 'test)                ; Faulty example
           (test "Terse" #:optional? "hello"))))         ; Faulty optional


;;;;; Tests for: option?

(test-assert "option?"
  (every (compose option? (cut <> 'name "Terse"))
         `(,private-option ,public-option ,open-option)))

(test-assert "option?-false"
  (not (any option?
            `(5 "test" (5 4 "blah") ,identity))))

;;;;; Tests for: configuration-file

(test-assert "configuration-file"
  (and (string=? (string-join '("/tmp" "name") file-name-separator-string)
                 (configuration-file (configuration 'name "terse" '()
                                       #:config-dir "/tmp")))
       (not (configuration-file (configuration 'name "terse" '())))))

;;;;; Tests for: complex-version
;;;
;;; Stand in for all version testing.



;;;;; Tests for: license-maker
;;;
;;; Stand in for all license testing.

(test-assert "license-maker"
  (every (compose (match-lambda
                    ((($ <prioption> 'license (? license?) license?)) #t)
                    (else #f))
                  (cut license-maker <> '()))
         `(gplv3+ agplv3+ "Test license." ,(license "id" "name" "url"))))

(test-assert "license-maker-fail"
  (every (error-catcher (cut license-maker <> '()) 'license-maker)
         `(symbol ,+ ,(private-option 'option "terse"))))

(test-end "spec")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; spec.scm ends here
