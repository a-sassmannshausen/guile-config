;; config.scm --- tests for config    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2015 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 23 November 2016
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
;; Unit tests for config.scm.
;;
;; Source-file: config.scm
;;
;;; Code:

(define-module (tests config)
  #:use-module (config)
  #:use-module (config api)
  #:use-module (config parser sexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (tests quickcheck)
  #:use-module (tests quickcheck-defs))


;;;; Tests

(test-begin "config")

(quickcheck-assert "Configurations?"
 configuration? ($configuration))

(quickcheck-assert
 "Getopt-Config?"
 (lambda (config)
   (codex? (getopt-config `(,(symbol->string
                              (configuration-name config)))
                          config)))
 ($configuration))

(quickcheck-assert
 "No config files created?"
 (lambda (config)
   (and (codex? (getopt-config `(,(symbol->string
                                   (configuration-name config)))
                               config))
        (not (file-exists?
              (path-given (configuration-directory config))))))
 ($configuration #:keywords (list $secret $switch) #:parser sexp-parser))

(quickcheck-assert
 "Config files created?"
 (lambda (config)
   (and (codex? (getopt-config `(,(symbol->string
                                   (configuration-name config)))
                               config))
        (cond ((null? (configuration-keywords config)) #t)
              ((file-exists? (path-given (configuration-directory config))) #t)
              (else #f))))
 ($configuration #:keywords (list $setting) #:parser sexp-parser))

(quickcheck-assert
 "Multi config files created?"
 (lambda (config)
   (and (codex? (getopt-config `(,(symbol->string
                                   (configuration-name config)))
                               config))
        (or (null? (configuration-keywords config))
            (every file-exists?
                   (map path-given (configuration-directory config))))))
 ($configuration #:keywords (list $setting) #:parser sexp-parser
                 #:directories
                 (($short-list
                   (lambda _
                     (path (given (string-append (tmpnam)
                                                 file-name-separator-string))
                           (eager? #t)))))))

(system "rm -r /tmp/file*")

(test-end "config")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
