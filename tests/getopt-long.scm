;; getopt-long.scm --- tests for getopt-long    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2019 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 24 January 2019
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
;; Unit tests for getopt-long.scm.
;;
;; Source-file: config/getopt-long.scm
;;
;;; Code:

(define-module (tests getopt-long)
  #:use-module (config)
  #:use-module (config api)
  #:use-module (config getopt-long)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))


;;;; Tests

(test-begin "config")

;; Bug: a switch's or setting's handler is invoked twice on the original
;; value. This can be measured with a procedure that contains state and which
;; sets n to that state.
(test-assert "keyword-handler-once"
  (= 1
     (option-ref
      (read-commandline
       '("script" "--test" "5") '()
       (codex #f (metadata "" "" #f '() "" #f #t #t #t #f)
              (valus `(,(switch (name 'test)
                                (test number?)
                                ;; Handler just tests how many times it is
                                ;; invoked and returns its count.
                                (handler (let ((x 0))
                                           (lambda (n)
                                             (set! x (1+ x))
                                             x)))))
                     '())
              (reagents '((script)) #f #f #f)))
      'test)))

(test-end "config")
