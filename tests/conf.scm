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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
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
  (let ((file (string-join '("/tmp" "test-config")
                           file-name-separator-string)))
    (when (file-exists? file)
      (delete-file file))
    (run-io ((@@ (conf) ensure-config-files)
             (configuration
              'test-config
              "Test configuration."
              `(,(open-option 'config
                              "Open, config file needed."
                              #:value "test"
                              #:test string?)
                ,(private-option 'priv
                                 "Private Option."
                                 #:value #f))
              #:config-dir "/tmp")))
    (file-exists? file)))

;;;;; Tests for: option-ref
(let* ((config (configuration
                'test-config
                "Test configuration."
                (list (public-option 'test
                                     "Value test."
                                     #:value "test"
                                     #:test string?)
                      (public-option 'target
                                     "Boolean test."
                                     #:value #f)
                      (private-option 'priv
                                      "Private Option."
                                      #:value #f)
                      (configuration 'subconf
                                     "A subconfiguration."
                                     (list
                                      (public-option 'verbose
                                                     "Boolean test."
                                                     #:value #f))
                                     #:alias 'sc))
                #:help? #t
                #:config-dir "/tmp"))
       (getopt (cut getopt-config <> config)))
  (test-equal "Simple param"
    (option-ref (getopt '("script" "--test" "hello")) 'test)
    "hello")
  (test-equal "Simple Boolean param"
    (option-ref (getopt '("script" "--target")) 'target)
    #t)
  (test-equal "Subconf Boolean param"
    (option-ref (getopt '("script" "subconf" "blah" "--verbose")) 'verbose)
    #t)
  (test-equal "Subconf positional param"
    (option-ref (getopt '("script" "subconf" "blah" "--verbose")) '())
    '("blah"))
  (test-equal "Subconf aliased positional param"
    (option-ref (getopt '("script" "sc" "blah" "--verbose")) '())
    '("blah"))
  (test-equal "Subconf aliased empty positional param"
    (option-ref (getopt '("script" "sc" "--verbose")) '())
    '())
  ;; ("script" "--test" "hello" "--verbose")  ; Bails out OK (no such opt)
  ;; ("script" "subconf" "blah" "--target")   ; Bails out OK (no such opt)
  )

;;;;; Tests for: inheritance-merge

(let* ((tersubconfig (configuration 'tersubconf
                                    "A subconfiguration."
                                    `(,(public-option 'bar
                                                      "number"
                                                      #:value 5
                                                      #:test number?))
                                    #:alias 'tsc))
       (secsubconfig (configuration 'secsubconf
                                    "A subconfiguration."
                                    `(,(open-option 'verbose
                                                      "Boolean test."
                                                      #:value #t)
                                      ,(private-option 'test
                                                      "Different test."
                                                      #:value "foo"
                                                      #:test string?)
                                      ,tersubconfig)
                                    #:alias 'ssc
                                    #:config-dir "/tmp"))
       (prisubconfig (configuration 'prisubconf
                                    "A subconfiguration."
                                    `(,(public-option 'verbose
                                                      "Boolean test."
                                                      #:value #f)
                                      ,secsubconfig)
                                    #:alias 'psc))
       (config (configuration
                'test-config
                "Test configuration."
                `(,(public-option 'test
                                  "Value test."
                                  #:value "test"
                                  #:test string?)
                  ,(public-option 'target
                                  "Boolean test."
                                  #:value #f
                                  #:inherit #f)
                  ,(private-option 'priv
                                   "Private Option."
                                   #:value #f)
                  ,prisubconfig)
                #:help? #t
                #:config-dir "/tmp")))
  ;; inheritance-merge demands at least one inheritance entry.
  ;; inheritance-merge simply merges what it finds in inheritance; it does not
  ;; care about inherit flags for its arguments!
  (test-assert "Inherit from root config, ignoring target"
    (match (configuration-options
            (run-io
             ((@@ (conf) inheritance-merge) prisubconfig
              `(,config))))
      ((('verbose . ($ <puboption> 'verbose #f))
        ('priv . ($ <prioption> 'priv #f))
        ('test . ($ <puboption> 'test "test"))
        ('help . ($ <puboption> 'help #f)))
       #t)
      (_ #f)))
  (test-assert "Inherit and override root (- target), first"
    (match (configuration-options
            (run-io
             ((@@ (conf) inheritance-merge) secsubconfig
              `(,config ,prisubconfig))))
      ((('verbose . ($ <openoption> 'verbose #t))
        ('priv . ($ <prioption> 'priv #f))
        ('test . ($ <prioption> 'test "foo"))
        ('help . ($ <puboption> 'help #f)))
       #t)
      (_ #f)))
  (test-assert "Inherit, override and add root (- target), first, second"
    (match (configuration-options
            (run-io
             ((@@ (conf) inheritance-merge) tersubconfig
              `(,config ,prisubconfig ,secsubconfig))))
      ((('bar . ($ <puboption> 'bar 5))
        ('verbose . ($ <openoption> 'verbose #t))
        ('priv . ($ <prioption> 'priv #f))
        ('test . ($ <prioption> 'test "foo"))
        ('help . ($ <puboption> 'help #f)))
       #t)
      (_ #f))))

;;;;; Tests for: find-subconfiguration

(let ((config (configuration
               'test-config
               "Test configuration."
               `(,(public-option 'test
                                 "Value test."
                                 #:value "test"
                                 #:test string?)
                 ,(public-option 'target
                                 "Boolean test."
                                 #:value #f)
                 ,(private-option 'priv
                                  "Private Option."
                                  #:value #f)
                 ,(configuration 'subconf
                                 "A subconfiguration."
                                 `(,(public-option 'verbose
                                                   "Boolean test."
                                                   #:value #f))
                                 #:alias 'sc)
                 ,(configuration 'secsubconf
                                 "A subconfiguration."
                                 `(,(public-option 'verbose
                                                   "Boolean test."
                                                   #:value #f))
                                 #:alias 'ssc))
                #:help? #t
                #:config-dir "/tmp")))
  (test-assert "Unknown subconfiguration"
      (catch 'find-subconfiguration
        (lambda () ((@@ (conf) find-subconfiguration) config 'missingconf))
        (lambda args #t)))
  (test-eqv "Find subconfiguration"
    (configuration-name ((@@ (conf) find-subconfiguration) config 'subconf))
    'subconf)
  (test-eqv "Find second subconfiguration"
    (configuration-name ((@@ (conf) find-subconfiguration) config 'secsubconf))
    'secsubconf))

;;;;; Tests for: inheritance-prune

(let ((configs `(,(configuration 'one "" '() #:inherit #f)
                 ,(configuration 'two "" '() #:inherit #t)
                 ,(configuration 'three "" '() #:inherit #t)
                 ,(configuration 'four "" '() #:inherit #t))))
  (test-assert (match ((@@ (conf) inheritance-prune) configs)
                 ((($ <configuration> 'one)) #t)
                 (_ #f))))

(let ((configs `(,(configuration 'one "" '() #:inherit #t)
                 ,(configuration 'two "" '() #:inherit #t)
                 ,(configuration 'three "" '() #:inherit #f)
                 ,(configuration 'four "" '() #:inherit #t))))
  (test-assert (match ((@@ (conf) inheritance-prune) configs)
                 ((($ <configuration> 'three) ($ <configuration> 'two)
                   ($ <configuration> 'one))
                  #t)
                 (_ #f))))

;;;;; Tests for: merge-config-file-values

(let* ((tersubconfig (configuration 'tersubconf
                                    "A subconfiguration."
                                    `(,(public-option 'bar
                                                      "number"
                                                      #:value 5
                                                      #:test number?))
                                    #:alias 'tsc #:inherit #t))
       (secsubconfig (configuration 'secsubconf
                                    "A subconfiguration."
                                    `(,(open-option 'verbose
                                                    "Boolean test."
                                                    #:value #t)
                                      ,(private-option 'test
                                                       "Different test."
                                                       #:value "foo"
                                                       #:test string?)
                                      ,tersubconfig)
                                    #:alias 'ssc
                                    #:config-dir "/tmp"))
       (prisubconfig (configuration 'prisubconf
                                    "A subconfiguration."
                                    `(,(public-option 'verbose
                                                      "Boolean test."
                                                      #:value #f)
                                      ,secsubconfig)
                                    #:alias 'psc #:inherit #t))
       (config (configuration
                'test-config
                "Test configuration."
                `(,(public-option 'test
                                  "Value test."
                                  #:value "test"
                                  #:test string?)
                  ,(public-option 'target
                                  "Boolean test."
                                  #:value #f)
                  ,(private-option 'priv
                                   "Private Option."
                                   #:value #f)
                  ,prisubconfig)
                #:help? #t
                #:config-dir "/tmp" #:inherit #t)))
  (test-equal "Merge values no subcommand"
    ;; Our options have no augmentation through inheritance!
    (configuration-options
     (run-io
      ((@@ (conf) merge-config-file-values) config '())))
    (configuration-options config))
  (test-assert "Merge values first subcommand: root inheritance"
    ;; We expect root config to be merged into first subconfig
    (match (configuration-options
            (run-io
             ((@@ (conf) merge-config-file-values)
              config '((prisubconf psc)))))
      ((('verbose . ($ <puboption> 'verbose #f))
        ('priv . ($ <prioption> 'priv #f))
        ('target  . ($ <puboption> 'target #f))
        ('test . ($ <puboption> 'test "test"))
        ('help . ($ <puboption> 'help #f)))
       #t)
      (_ #f)))
  (test-equal "Merge values second subcommand: destroy inheritance"
    ;; Our options have no augmentation through inheritance!
    (configuration-options
     (run-io
      ((@@ (conf) merge-config-file-values)
       config '((prisubconf psc) (secsubconf ssc)))))
    (configuration-options secsubconfig))
  (test-assert "Merge values third subcommand: secsubconfig inheritance"
    ;; We expect root config to be merged into first subconfig
    (match (configuration-options
            (run-io
             ((@@ (conf) merge-config-file-values)
              config '((prisubconf psc) (secsubconf ssc) (tersubconf tsc)))))
      ((('bar . ($ <puboption> 'bar 5))
        ('test . ($ <prioption> 'test "foo"))
        ('verbose . ($ <openoption> 'verbose #t)))
       #t)
      (e #f))))

(test-end "config")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; config.scm ends here
