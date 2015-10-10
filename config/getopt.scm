;;; Config Getpot --- Configuration Getopt-Long intersection
;;; Copyright © 2015 Alex Sassmannshausen <alex@pompo.co>
;;;
;;; This file is part of Config.
;;;
;;; Config is free software; you can redistribute it and/or modify it under
;;; the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Config is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with glean; if not, contact:
;;;
;;; Free Software Foundation           Voice:  +1-617-542-5942
;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (config getopt)
  #:use-module (config config-spec)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export     (derive/merge-config-getopt
                configuration->getopt-spec))

;;;; Configuration/Getopt Merging

(define (derive/merge-config-getopt config cli-params)
  "Return the <configuration> resulting from merging the list CLI-PARAMS
(normally the list of commandline arguments to a program) into <configuration>
CONFIG."
  (merge-config-getopt config
                       (getopt-long cli-params
                                    (configuration->getopt-spec config))))

(define (merge-config-getopt config getopts)
  "Return the <configuration> resulting from merging the getopt-long interface
GETOPTS (normally the list of commandline arguments to a program) into
<configuration> CONFIG."
  (set-configuration-values config
                            (map (getopt-merger getopts)
                                 (configuration-values config))))

(define (getopt-merger getopts)
  "Return a procedure taking a configuration-value from a <configuration>,
which returns an updated configuration-value if an updated values is present
in the getopt-long interface GETOPTS.  Otherwise that procedure will simply
return the original configuration-value."
  (lambda (config-val)
    (match config-val
      (($ <puboption> name)
       (opt-if set-puboption-value (option-ref getopts name #f) config-val))
      (($ <openoption> name)
       (opt-if set-openoption-value (option-ref getopts name #f) config-val))
      (_ config-val))))

(define (opt-if setter getopt option)
  "Return either a new configuration-value built with SETTER, OPTION and
GETOPT, or just the original configuration-value OPTION."
  (if getopt (setter option getopt) option))


;;;; Configuration Spec parsing

;;;;; Option Parsing

(define (configuration->getopt-spec config)
  "Return the getopt-long option-spec corresponding to CONFIG.  Any private
options will be ignored, as they have no getopt-long config (they have no
commandline capability)."
  (filter identity (map option->getopt-spec (configuration-values config))))

(define (option->getopt-spec option)
  "Return the getopt-long option-spec for OPTION, or #f if it does not
apply."
  (match option
    ((? public-option?) (puboption->getopt-spec option))
    ((? private-option?) #f)
    ((? open-option?)  (openoption->getopt-spec option))))

(define* (getopt-spec name test single-char #:optional required)
  "Create a getopt-long spec entry from NAME, TEST, SINGLE-CHAR and
REQUIRED."
  (define (value-entry)
    (match (procedure-name test)
      ('boolean? '((value #f)))
      (_         '((value #t)))))

  (apply list name `(predicate ,test) `(required? ,required)
         (match single-char
           ((? char?) (cons `(single-char ,single-char)
                           (value-entry)))
           (#f        (value-entry)))))

(define (openoption->getopt-spec openoption)
  "Return the getopt-long option-spec for OPENOPTION."
  (match openoption
    (($ <openoption> name '<unset> cli-test _ single-char _ _)
     (getopt-spec name cli-test single-char #t))
    (($ <openoption> name _ cli-test _ single-char _ _)
     (getopt-spec name cli-test single-char))))

(define (puboption->getopt-spec puboption)
  "Return the getopt-long option-spec for PUBOPTION."
  (match puboption
    (($ <puboption> name '<unset> test single-char _ _)
     (getopt-spec name test single-char #t))
    (($ <puboption> name _ test single-char _ _)
     (getopt-spec name test single-char))))

;;; getopt.scm ends here.
