;; examples/hello-world.scm --- hello-world implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;;
;; This file is part of guile-examples.
;;
;; guile-examples is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-examples is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with guile-examples; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;; Code:

(define-module (hello-world)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (config)
  #:use-module (config api)
  #:use-module (config parser sexp)
  #:export ())

(define config
  ;; Define our root configuration
  (configuration
   (name 'hello-world)
   (keywords
    (list
     ;; Switch to force writing non-eager configuration files
     (switch
      (name 'write) (default #f) (test boolean?) (character #f)
      ;; Boolean Switch ---^-----------^          ^--- No single character
      (synopsis "Write configuration file in local dir."))
     (switch
      (name 'age) (default 19)
      ;; We declare this be passed as string and be turned into a number.
      (handler string->number) (test number?) (example "15")
      (synopsis "Your age."))
     ;; A setting in the configuration file, if it exists.
     (setting
      (name 'welcome) (default "Welcome people!")
      (handler identity) (test string?)
      (synopsis "The text to print on the first line.")
      (description "Our hello-world program is very versatile.  Adjust this
setting to have the program print an entirely different message!"))
     (setting
      (name 'outro) (default "I was too short for this beautiful world!")
      (handler identity) (test string?) (example "Goodbye")
      (synopsis "The text to print on the final line."))))
   (synopsis "Print customizable welcome messages")
   (description "Hello World is a Guile implementation of the famous simple C
program.  This script shows off the versatility of using settings and
switches, granting the end-user full control over their application.")
   ;; We have subcommands!
   (subcommands
    (list
     (configuration
      (name 'bye-world)
      ;; Short name
      (alias 'bye)
      ;; We want to inherit all options
      (wanted '((keywords . (age welcome outro write))))
      (synopsis "A subcommand!"))))
   (parser sexp-parser)
   ;; Specify where we want to install configuration files
   (directory (list
               ;; In the user's home directory, under .hello-world.  This one
               ;; is eager.
               (in-home ".hello-world/")
               ;; In the directory in which the command is invoked.  This one
               ;; is lazy.
               (in-cwd ".config/")))))

(define (main cmd-line)
  (let ((options (getopt-config-auto cmd-line config)))
    (when (option-ref options 'write)
      (options-write options))
    (format #t "~a~%  - ~a~%~a~%"
            (option-ref options 'welcome)
            (match (option-ref options 'age (empty))
              (($ <empty>) "What is your age?")
              (e (string-append "You were born in " 
                                (number->string (- 2018 e)) ".")))
            (option-ref options 'outro))))

(main (command-line))
