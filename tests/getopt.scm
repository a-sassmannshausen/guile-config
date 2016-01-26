;; getopt.scm --- tests for getopt    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex@pompo.co>
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
;; Unit tests for getopt.scm.
;;
;; Source-file: conf/getopt.scm
;;
;;; Code:

(define-module (tests getopt)
  #:use-module (conf)
  #:use-module (conf getopt)
  #:use-module (conf spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  )

(define test-config
  (configuration 'test-config
    "Test configuration."
    (list (public-option 'bool
            "A to be flattened option"
            #:value #f)
          (private-option 'pritest
                  "A private test, this should be ignored."
                  #:value "hello"
                  #:test string?)
          (public-option 'num
            "A to be flattened option"
            #:value 0
            #:test number?
            #:single-char #\n
            #:handler string->number)
          (public-option 'csv
            "A to be flattened option"
            #:value '("hello" "world")
            #:test (match-lambda
                     (((? string?) ...) #t)
                     (_ #f))
            #:handler (lambda (csv)
                        (string-split csv #\,)))
          (open-option 'obool
            "Open option"
            #:value #f)
          (open-option 'onum
            "A to be flattened option"
            #:value 0
            #:test number?
            #:handler string->number)
          (open-option 'ocsv
            "A to be flattened option"
            #:value '("hello" "world")
            #:test (match-lambda
                     (((? string?) ...) #t)
                     (_ #f))
            #:handler (lambda (csv)
                            (string-split csv #\,))))
    #:config-dir "/tmp"))


;;;; Tests

(test-begin "getopt")

;;;;; Tests for: config->getopt-long

(test-assert "Getopt-long conversion"
  (every (lambda (input output)
           (equal? ((@@ (conf getopt) config->getopt-long) test-config input)
                   output))
         ;; We cannot test for unexpected inputs, as getopt-long will exit
         ;; when predicate fails.
         ;; We cannot test for unknown arguments, as getopt-long will exit in
         ;; such a case.
         ;; Commandline inputs
         '(("command" "--bool" "--num" "8" "--csv" "bye,planet")
           ("command" "--bool" "-n" "8" "--csv" "bye,planet")
           ("command" "--obool" "--onum" "8" "--ocsv" "bye,planet")
           ("command" "bla" "hooray" "--obool" "--onum" "8"))
         ;; Expected outputs
         '(((()) (csv . "bye,planet") (num . "8") (bool . #t))
           ((()) (csv . "bye,planet") (num . "8") (bool . #t))
           ((()) (ocsv . "bye,planet") (onum . "8") (obool . #t))
           ((() . ("bla" "hooray")) (onum . "8") (obool . #t)))))

;;;;; Tests for: derive-free-params

(test-assert "Derive-free-params"
  (every (lambda (input output)
           (equal? 
            ((@@ (conf getopt) derive-free-params)
             (configuration 'test
               "Test config."
               (list
                (public-option 'root-bool
                  "Root bool option."
                  #:value #f)
                (private-option 'pritest
                  "A private test, this should be ignored."
                  #:value "hello"
                  #:test string?)
                (configuration 'level2
                  "Level 2 config."
                  (list
                   (public-option 'level2-bool
                     "Level 2 bool option."
                     #:value #t)
                   (public-option 'level2-number
                     "Level 2 Number option."
                     #:value 5
                     #:handler string->number
                     #:test number?)
                   (configuration 'level3
                     "Level 3 config."
                     (list
                      (public-option 'level3-bool
                        "Level 3 bool option."
                        #:value #t)))))))
             input)
            output))
         ;; Input
         '(("command" "test" "free" "level2" "level3" "hello")
           ("command")
           ("command" "test" "--root-bool" "hello" "--level2-number" "7"
            "world"))
         ;; Output
         '(("test" "free" "level2" "level3" "hello")
           ()
           ("test" "hello" "world")))) 

;; Boolean, then value taking, problem
(test-assert "Derive-free-params boolean/value-taking problem"
  ((@@ (conf getopt) derive-free-params)
   (configuration 'test
     "Test config."
     (list
      (public-option 'root-bool
        "Root bool option."
        #:value #f)
      (configuration 'hidden-subcommand
        "Level 2 config."
        (list
         (public-option 'root-bool
           "Root bool option."
           #:value 5
           #:handler string->number
           #:test number?)))))
   '("command" "--root-bool" "false-free-param" "hidden-subcommand")))

;; Value-taking, then boolean, problem.
(test-assert "Derive-free-params value-taking/boolean problem"
  ((@@ (conf getopt) derive-free-params)
   (configuration 'test
     "Test config."
     (list
      (public-option 'root-bool
        "Root bool option."
        #:value 5
        #:handler string->number
        #:test number?)
      (configuration 'disappeared-subcommand
        "Level 2 config."
        (list
         (public-option 'root-bool
           "Root bool option."
           #:value #f)
         ))))
   '("command" "--root-bool" "disappeared-subcommand")))

;;;;; Tests for: expand-configs

(test-assert "Expand-configs"
  (match ((@@ (conf getopt) expand-configs)
          `((level1 . ,(configuration 'level1
                         "Level 1 config."
                         '()
                         #:alias 'lvl1))
            (level2 . ,(configuration 'level2
                         "Level 2 config."
                         (list
                          (public-option 'level2-bool
                            "Level 2 bool option."
                            #:value #t)
                          (public-option 'level2-number
                            "Level 2 Number option."
                            #:value 5
                            #:handler string->number
                            #:test number?))))
            (level3 . ,(configuration 'level3
                         "Level 3 config."
                         (list
                          (public-option 'level3-bool
                            "Level 3 bool option."
                            #:value #t))
                         #:alias 'lvl3))))
    
    ((('lvl3 . (? configuration?))
      ('level3 . (? configuration?))
      ('level2 . (? configuration?))
      ('lvl1 . (? configuration?))
      ('level1 . (? configuration?))) #t)
    (_ #f)))

;;;;; Tests for: establish-subcommands

(test-assert "Establish subcommands"
  (every (lambda (input output)
           (equal? (establish-subcommands
                    (configuration 'test
                      "Test config."
                      (list
                       (configuration 'level2
                         "Level 2 config."
                         (list
                          (public-option 'hidden-option
                            "A root-level hidden option."
                            #:value #t)
                          (configuration 'level3
                            "Level 3 config."
                            (list
                             (public-option 'hidden-option
                               "A root-level hidden option."
                               #:value #t)))))))
                    input)
                   output))
         '(("Script-name" "--hidden-option" "level3" "level2")
           ("Script-name" "--hidden-option" "level2")
           ("Script-name" "--hidden-option" "level2" "level3"))
         '(()
           (level2)
           (level2 level3))))

;;;;; Tests for: derive/merge-config-getopt

(test-assert "derive/merge-config-getopt"
  (every (lambda (in test)
           (test (derive/merge-config-getopt test-config in)))
         '(("command" "--bool" "--num" "8" "--csv" "bye,planet")
           ("command" "--bool" "-n" "8" "--csv" "bye,planet")
           ("command" "--obool" "--onum" "8" "--ocsv" "bye,planet")
           ("command" "bla" "hooray" "--obool" "--onum" "8"))
         `(,(lambda (getopt)
              (let ((opt (lambda (name) (option-ref getopt name #f))))
                (and (opt 'bool)
                     (not (opt 'obool))
                     (= (opt 'num) 8)
                     (equal? (opt 'csv) '("bye" "planet")))))
           ,(lambda (getopt)
              (let ((opt (lambda (name) (option-ref getopt name #f))))
                (and (opt 'bool)
                     (not (opt 'obool))
                     (= (opt 'num) 8)
                     (equal? (opt 'csv) '("bye" "planet")))))
           ,(lambda (getopt)
              (let ((opt (lambda (name) (option-ref getopt name #f))))
                (and (opt 'obool)
                     (not (opt 'bool))
                     (= (opt 'onum) 8)
                     (equal? (opt 'ocsv) '("bye" "planet")))))
           ,(lambda (getopt)
              (let ((opt (lambda (name) (option-ref getopt name #f))))
                (and (not (opt 'bool))
                     (opt 'obool)
                     (= (opt 'onum) 8)))))))

;;;;; Tests for: openoption->getopt-spec

(test-assert "openoption->getopt-spec"
  (every (lambda (in test)
           (test ((@@ (conf getopt) openoption->getopt-spec) in)))
         ;; Inputs
         (list
          (open-option 'test "" #:single-char #\t #:value #f)
          (open-option 'test "" #:single-char #\t #:value '<unset>)
          (open-option 'tst "" #:value 5 #:test number?
            #:handler string->number)
          (open-option 'optional "" #:value #f
            #:test (match-lambda
                     ((or #f #t (? number?)) #t)
                     (_ #f))
            #:handler (match-lambda
                        ((? boolean? b) b)
                        ((? string? str) (string->number str)))
            #:optional? #t))
         ;; Outputs
         (list
          (match-lambda
            (('test ('predicate (? procedure?))
                    ('required? #f)
                    ('single-char #\t)
                    ('value #f))
             #t)
            (_ #f))
          (match-lambda
            (('test ('predicate (? procedure?))
                    ('required? #t)
                    ('single-char #\t)
                    ('value #f))
             #t)
            (_ #f))
          (match-lambda
            (('tst ('predicate (? procedure?))
                   ('required? #f)
                   ('value #t))
             #t)
            (_ #f))
          (match-lambda
            (('optional ('predicate (? procedure?))
                        ('required? #f)
                        ('value 'optional))
             #t)
            (_ #f)))))

;;;;; Tests for: puboption->getopt-spec

(test-assert "puboption->getopt-spec"
  (every (lambda (in test)
           (test ((@@ (conf getopt) puboption->getopt-spec) in)))
         ;; Inputs
         (list
          (public-option 'test "" #:single-char #\t #:value #f)
          (public-option 'test "" #:single-char #\t #:value '<unset>)
          (public-option 'tst "" #:value 5 #:test number?
            #:handler string->number)
          (public-option 'optional "" #:value #f
            #:test (match-lambda
                     ((or #f #t (? number?)) #t)
                     (_ #f))
            #:handler (match-lambda
                        ((? boolean? b) b)
                        ((? string? str) (string->number str)))
            #:optional? #t))
         ;; Outputs
         (list
          (match-lambda
            (('test ('predicate (? procedure?))
                    ('required? #f)
                    ('single-char #\t)
                    ('value #f))
             #t)
            (_ #f))
          (match-lambda
            (('test ('predicate (? procedure?))
                    ('required? #t)
                    ('single-char #\t)
                    ('value #f))
             #t)
            (_ #f))
          (match-lambda
            (('tst ('predicate (? procedure?))
                   ('required? #f)
                   ('value #t))
             #t)
            (_ #f))
          (match-lambda
            (('optional ('predicate (? procedure?))
                        ('required? #f)
                        ('value 'optional))
             #t)
            (_ #f)))))

;;;;; Tests for: configuration->getopt-spec

(test-assert "configuration->getopt-spec"
  (match (configuration->getopt-spec test-config)
    ((('bool ('predicate (? procedure?))
             ('required? #f)
             ('value #f))
      ('num ('predicate (? procedure?))
            ('required? #f)
            ('single-char #\n)
            ('value #t))
      ('csv ('predicate (? procedure?))
            ('required? #f)
            ('value #t))
      ('obool ('predicate (? procedure?))
              ('required? #f)
              ('value #f))
      ('onum ('predicate (? procedure?))
             ('required? #f)
             ('value #t))
      ('ocsv ('predicate (? procedure?))
             ('required? #f)
             ('value #t)))
     #t)
    (_ #f)))

(test-end "getopt")

;;; getopt.scm ends here
