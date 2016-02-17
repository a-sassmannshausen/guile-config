;; config.scm --- tests for config    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2015 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 23 November 2015
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
;; Unit tests for conf.scm.
;;
;; Source-file: conf.scm
;;
;;; Code:

(define-module (tests conf)
  #:use-module (conf)
  #:use-module (conf spec)
  #:use-module (ice-9 match)
  #:use-module (conf monads)
  #:use-module (conf monads io)
  #:use-module (srfi srfi-64)
  )


;;;; Tests

(test-begin "config")

;;;;; Tests for: configuration

(test-assert "Configuration failures"
  (catch 'config-spec
    (lambda ()
      (not (configuration 'test-configuration
             "A test configuration with a terse longer than 40 chars."
             '())))
        (lambda (k t)
          (string=? "TERSE should be a string shorter than 40." t))))

(test-assert "Configuration creation."
  (match (configuration 'test-configuration
           "A test configuration."
           (list
            (private-option 'test-priopt
              "A test option"
              #:value 5)
            (public-option 'test-pubopt
              "A test option"
              #:value 5)
            (open-option 'test-openopt
              "A test option"
              #:value 5))
           #:config-dir "/tmp"
           #:long "Long info on config."
           #:help? #t
           #:usage? #t
           #:version? "0.1"
           #:license 'agplv3+
           #:copyright '(2000 2001 2014)
           #:author "Alex Sassmannshausen")
    (($ <configuration> (? symbol? n) (? string? d)
                        ((name . (? option? opts)) ...)
                        ((name . (? configuration? confs)) ...)
                        (? string? t)
                        (? string? l)
                        parser) #t)
    (_ #f)))

;; Make sure we don't require a configuration file if we have no need for it.
(test-assert "Configuration no config ok."
  ;; Config-dir, but no open-options -> no need for it
  (and (run-io ((@@ (conf) merge-config-file-values)
                (configuration 'test-config
                  "Test configuration."
                  (list (public-option 'no-config
                          "Pub, no config file needed."
                          #:value 'test)
                        (private-option 'priv
                          "Private Option."
                          #:value 1))
                  #:config-dir "/tmp")
                '()))
       ;; No config-dir, and no open-options
       (run-io ((@@ (conf) merge-config-file-values)
                (configuration 'test-config
                  "Test configuration."
                  (list (public-option 'no-config
                          "Pub, no config file needed."
                          #:value 'test)
                        (private-option 'priv
                          "Private Option."
                          #:value 1)))
                '()))))

;;;;; Tests for: sort-subcommands

(test-equal "Sort Subcommands"
  "  a     a\n  b     b\n  c     c\n  d     d"
  ((@@ (conf) sort-subcommands)
   `((d . ,(configuration 'd "d" '()))
     (c . ,(configuration 'c "c" '()))
     (b . ,(configuration 'b "b" '()))
     (a . ,(configuration 'a "a" '())))))

(test-equal "Sort Subcommands with alias"
  "  alpha        a\n  bravo        b\n  charlie      c\n  delta   | d  d"
  ((@@ (conf) sort-subcommands)
   `((d . ,(configuration 'delta "d" '() #:alias 'd))
     (c . ,(configuration 'charlie "c" '()))
     (b . ,(configuration 'bravo "b" '()))
     (a . ,(configuration 'alpha "a" '())))))

;;;;; Tests for: getmio-config-auto

;; We can't test configurations with <unset> values: getopt currently just
;; exits when mandatory param is missing.
(test-assert "Generate options"
  (run-io (getmio-config-auto
           '("script-name")
           (configuration 'test-config
             "Test configuration."
             (list (public-option 'no-config
                     "Pub, no config file needed."
                     #:value #f)
                   (private-option 'priv
                     "Private Option."
                     #:value #f))
             #:help? #t
             #:config-dir "/tmp"))))

;; Test configuration with <unset> value passed in through commandline-args.
(test-assert "Mandatory arg present."
  (run-io (getmio-config-auto
           '("script-name" "--no-config" "5")
           (configuration 'test-config
             "Test configuration."
             (list (public-option 'no-config
                     "Pub, no config file needed."
                     #:test string?)
                   (private-option 'priv
                     "Private Option."
                     #:value #f))
             #:help? #t
             #:config-dir "/tmp"))))

;;;;; Tests for: ensure-config-files

(test-assert "Create config-file"
  (begin
    (run-io ((@@ (conf) ensure-config-files)
             (configuration 'test-config
               "Test configuration."
               (list (open-option 'config
                       "Open, config file needed."
                       #:value "test"
                       #:test string?)
                     (private-option 'priv
                       "Private Option."
                       #:value #f))
               #:help? #t
               #:config-dir "/tmp")))
    (file-exists? (string-join '("/tmp" "test-config")
                               file-name-separator-string))))

(test-end "config")

;;; config.scm ends here
