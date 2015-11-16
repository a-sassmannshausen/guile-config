;; config.scm --- The config module    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 12 July 2015
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
;; The '(config config) library provides high-level easy to use,
;; functional (to customizable degrees) application configuration.
;;
;;; Code:

;;;; A summary (duplicated below):
;;;
;;; This bit should just serialize our IO operations, after which we
;;; should use ordinary scheme in our program: just pass the relevant
;;; bit of information to our functions.
;;; So:
;;; - monad:
;;;   + creates config files,
;;;   + parses config files,
;;;   + parses args,
;;;   + creates config object,
;;;   + runs help/usage/version (which returns nothing 'output)
;;;   + exit on (nothing 'output), else return configuration.
;;; - ordinary scheme (e.g. let):
;;;   + receives it,
;;;   + extract values, map to appropriate functions.
;;;
;;; The scope of the library is exactly the monad.
;;;
;;; Note:
;;;
;;; As a first approximation we are not implementing recursive configurations
;;; (i.e. sub-commands).
;;;
;;; Monadic logic:
;;; 

(define-module (config)
  #:use-module (config getopt)
  #:use-module (config parser)
  #:use-module (config spec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (monads)
  #:use-module (monads io)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:re-export  (define-private-option define-public-option define-open-option
                 configuration-parser configuration-print)
  #:export     (
                getopt-config
                getopt-config-auto
                getmio-config
                getmio-config-auto
                option-ref
                make-help-emitter
                make-version-emitter
                define-configuration
                ))


;;;; Porcelain

(define (getmio-config args configuration)
  "Return a monadic IO value, which, when evaluated, returns the
<configuration> derived from merging CONFIGURATION, ARGS and CONFIGURATION's
configuration files."
  (mlet* %io-monad
      ;; Remember, we may have nested configurations!
      (;; first create getopt-long
       ;; then check '() of getopt-long for recursive configurations
       ;; if present, merge each wich is present
       ;; if not, proceed as below.
       ;; ----
       ;; will need to be lifted when implementing above!
       (ignore (ensure-config-files configuration))
       (merged-config (merge-config-file-values configuration)))
    ;; derive/merge-config-getopt is non-monadic!
    (return (derive/merge-config-getopt merged-config args))))

(define (getmio-config-auto args config)
  "Return a monadic IO value, which, when evaluated, returns the
<configuration> derived from merging CONFIG, ARGS and CONFIG's configuration
files.

Prior to returning <configuration> check whether --help, --usage or --version
was passed, and if it was, emit the appropriate messages before exiting."
  (mlet* %io-monad
      ((parsed (getmio-config args config))
       (emit   ((io-lift (lambda (port)
                (when (or (option-ref parsed 'help)
                          (option-ref parsed 'usage))
                  (make-help-emitter parsed port)
                  (exit 0))
                (when (option-ref parsed 'version #f)
                  (make-version-emitter parsed port)
                  (exit 0)))
              'output))))
    (return parsed)))

(define* (getopt-config args config #:key (input-port (current-input-port))
                       (output-port (current-output-port))
                       (error-port (current-error-port)))
  "Return the <configuration> derived from merging CONFIG, ARGS and CONFIG's
configuration files."
  (run-io (getmio-config args config) input-port output-port error-port))

(define* (getopt-config-auto args config #:key
                             (input-port (current-input-port))
                             (output-port (current-output-port))
                             (error-port (current-error-port)))
  "Return the <configuration> derived from merging CONFIG, ARGS and CONFIG's
configuration files.

Prior to returning <configuration> check whether --help, --usage or --version
was passed, and if it was, emit the appropriate messages before exiting."
  (run-io (getmio-config-auto args config) input-port output-port error-port))

(define* (option-ref configuration key #:optional default)
  "Return the value for KEY in CONFIGURATION, or DEFAULT if it cannot be
found."
  (match configuration
    (($ <configuration> _ _ values _ _)
     (match (assq (if (null? key) 'the-empty-prioption key) values)
       (#f default)
       ((k . v) (match v
                  (($ <puboption> n v) v)
                  (($ <prioption> n v) v)
                  (($ <openoption> n v) v)))))))

(define* (make-help-emitter config #:optional port)
  "Traverse CONFIG, building a GNU-style help message as we do so and emit it
to PORT."
  (match (configuration-values config)
    (((names . (? option? opts)) ...)
     ;; Short Help
     (format port "Usage: ~a ~a~%"
             (symbol->string (configuration-name config))
             (sort-opts (filter (negate private-option?) opts)
                        (+ (string-length "Usage: ")
                           (string-length (symbol->string
                                           (configuration-name config)))
                           1)))
     ;; Detailed Help
     (format port "~%Options:~%~a~%"
             (sort-detailed-opts (filter (negate private-option?) opts)))
     ;; Synopsis
     (if (configuration-long config)
         (format port "~%~a~%"
                 (fill-paragraph (configuration-long config) 80))))

    (((name . (or ($ <configuration>) (? option? opts))) ...)
     (throw 'config "CONFIGURATION in help is not yet supported."))
    (_ (throw 'config "CONFIG is invalid."))))

(define* (make-version-emitter config #:optional port)
  "Traverse CONFIG, building a GNU-style version message as we do so and emit
it to PORT."
  (format port "~a~%~a~a~a~%"
          (match (list (configuration-name config)
                       (option-ref config 'version-number #f))
            ((name #f) (symbol->string name))
            ((name (? string? version))
             (string-append (symbol->string name) " " version))
            ((name (? number? version))
             (string-append (symbol->string name) (number->string version))))
          (match (map (cut option-ref config <> #f) '(copyright author))
            ((or (#f _) (_ #f)) "")
            ((years author)
             (string-append "Copyright (C) "
                            (string-join (map number->string years) ", ")
                            " " author "\n")))
          (match (option-ref config 'license #f)
            ((? license? license)
             (string-append (license->string license) "\n"))
            (_ ""))
          "This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."))

(define* (define-configuration name terse values #:key config-dir
           long help? usage? version? license copyright author
           (version-test? string?) (parser simple-parser))
  "Return a configuration.  NAME should be a symbol naming the configuration.
TERSE is a < 40 char description; VALUES is a list of config-options.  The
optional arguments:
 - CONFIG-DIR: the directory in which a configuration file should be
generated.
 - LONG: a longer documentation string (mainly used in config files).
 - HELP?: if #t, add a public help option to VALUES.
 - USAGE?: if #t, add a public usage option to VALUES.
 - LICENSE: a symbol or string defaulting to the symbol 'gplv3+.  If this is a
string it will be the \"License:\" line in '--version' output.
 - AUTHOR: a string naming the author of the project.
 - COPYRIGHT: a list of years for which the copyright applies.
 - VERSION?: if a value, add a private version-number option to VALUES,
populated with the value for this option.  We will also create a public
version option in VALUES.
 - VERSION-TEST?: a procedure used to validate the version number value.
If omitted, this will default to `string?'.
 - PARSER: the configuration file parser we will use to write and read the
configuration file associated with this configuration.  It defaults to
SIMPLE-PARSER."
  ;; If we have been provided with convenience option values, we should
  ;; augment our configuration-values before finally instantiating
  ;; <configuration>.
  (define (augment-if proc do? values)
    (match do?
      (#t (cons (proc) values))
      (#f values)
      (_ (match (proc do? version-test?)
           ((vrsion vrsion-num) (cons* vrsion vrsion-num values))))))
  (define* (augment-values values #:optional next)
    (define (version-augment values)
      (list (if version?
                (match (complex-version version? version-test?)
                  ((vrsion vrsion-num)
                   (cons* vrsion vrsion-num values))
                  (_ (throw 'config-spec
                            "This should really not have happened.")))
                values)
            license-augment))
    (define (license-augment values)
      (list (if license (license-maker license values) values)
            copyright-augment))
    (define (copyright-augment values)
      (list (match copyright
              (#f values)
              (((? integer? years) ...) (cons (copyright-maker years) values))
              (_ (throw 'config-spec
                        "Invalid COPYRIGHT: should be a list of years.")))
            author-augment))
    (define (author-augment values)
      (list (match author
              (#f values)
              ((? string?) (cons (author-maker author) values))
              (_ (throw 'config-spec
                        "Invalid AUTHOR: should be a string.")))
            #t))

    (match next
      ((? procedure?) (apply augment-values (next values)))
      (#f (apply augment-values (version-augment values)))
      (#t values)))

  ;; We generate a <configuration> record, but only if we pass our basic
  ;; parsing of the values that were provided.
  (mecha-configuration
   (check-name name)
   (match config-dir
     ((and (? string?) (? absolute-file-name?)) config-dir)
     (#f (match values
           (((and (? option?) (? (negate open-option?))) ...) config-dir)
           (_ (throw 'config-spec
                    "If not CONFIG-DIR, then no openoptions are allowd."))))
     ;; Else error value
     (_ (throw 'config-spec "CONFIG-DIR should be an absolute filepath, or #f.")))
   (match values
     (((or (? option?) (? configuration?)) ...)
      (map (lambda (opt/conf)
             "Turn OPT/CONF into a k/v pair where k is the name of opt/conf."
             (match opt/conf
               (($ <configuration> name) (cons name opt/conf))
               (($ <prioption> name) (cons name opt/conf))
               (($ <puboption> name) (cons name opt/conf))
               (($ <openoption> name) (cons name opt/conf))
               (_ (throw 'config-spec "Invalid value in configuration."))))
           ;; Generate full list of values
           ;; Starting with the special option for non-opts or opt-values
           (cons (define-private-option 'the-empty-prioption
                   "Special empty list private property."
                   #:value '()
                   #:test list?)
                 (augment-values
                  (augment-if usage usage?
                              (augment-if help help? values))))))
     ;; Else error value
     (_ (throw 'config-spec
               "VALUES should be a list of options and/or configurations.")))
   (match terse
     ;; FIXME: Also test whether shorter than max-length!
     ((? string?) terse)
     (_ (throw 'config-spec "TERSE should be a string.")))
   (match long
     ((or (? string?) #f) long)
     (_ (throw 'config-spec "LONG should be a string, or #f.")))
   (match parser
     ((? configuration-parser?) parser)
     (_ (throw 'config-spec "PARSER should be a configuration parser.")))))


;;;; Plumbing

;; => io read -> configuration.
(define (merge-config-file-values configuration)
  "Return a version of CONFIGURATION which has been augmented by the
configuration values from its configuration file."
  (mlet* %io-monad
      ((old-io (set-io-input-file (configuration-file configuration)))
       (config (configuration-read configuration))
       (ignore (io-close-input-port old-io)))
    (return config)))

;; => io write -> '()
(define (ensure-config-files configuration)
  "Check if CONFIGURATION's config-file exists, and create it if it doesn't.
Return values is unspecified in the io-monad."
  (define (ensure config)
    (if (not (file-exists? (configuration-file config)))
        (mlet* %io-monad
            ((ignore  (iomkdir-p (configuration-dir config)))
             (old-out (set-io-output-file (configuration-file config)))
             (ignore  (configuration-write config)))
          (io-close-output-port old-out))
        (with-monad %io-monad (return '()))))
  (define (recurse values)
    (match values
      (() (with-monad %io-monad (return '())))
      (((name . (? configuration? config)) . rest)
       ;; create config, then recurse with (configuration-values config) + rest
       (mlet* %io-monad
           ((ignore (ensure config))
            (ignore (recurse (configuration-values config))))
         (recurse rest)))
      ((option . rest) (recurse rest))))

  (mlet* %io-monad
      ((ignore (ensure configuration)))
    (recurse (configuration-values configuration))))


;;;; Helpers

(define (sort-detailed-opts opts)
  ;; Long options should be:
  ;; - Determine longest option name
  ;; - Sorted alpha, one per line, with long name padded to longest
  ;; Options:
  ;;   --name     -n   Name of user
  ;;   --target   -t   Target of game
  ;;   --zulu          Bogus option
  (define (padded n longest)
    (let moar ((name n)
               (padding (- longest (string-length n))))
      (if (> padding 0)
          (moar (string-append name " ") (1- padding))
          name)))
  (string-join
   (match (fold (lambda (opt result)
                  ;; result: `(opts longest)
                  (match result
                    ((opts . longest)
                     (match opt
                       ((or ($ <puboption> n _ _ _ s t)
                            ($ <openoption> n _ _ _ _ s t))
                        (match (symbol->string n)
                          ((? (compose (cut > <> longest) string-length) n)
                           (cons (cons (list n s t) opts) (string-length n)))
                          (n (cons (cons (list n s t) opts) longest))))))))
                '(() . 0)
                (filter (negate private-option?) opts))
     ((opt-specs . longest)
      (sort (map (lambda (spec)
                   (match spec
                     ((n s t)
                      (string-append "  --" (padded n longest)
                                     (if s
                                         (string-append "  -" (string s))
                                         "    ")
                                     "  " t))))
                 opt-specs)
            string-ci<=?)))
   "\n"))

(define (sort-opts opts indent)
  (define (whitespace)
    (let moar ((togo indent)
               (white ""))
      (if (> togo 0)
          (moar (1- togo) (string-append " " white))
          white)))
  (define (boolproc? proc) (eq? 'boolean? (procedure-name proc)))
  (string-join
   (filter (cut (negate string=?) "" <>)
           ((lambda (almost-sorted-sets)
              (match almost-sorted-sets
                ((short-bools long-bools short-rest long-rest)
                 (cons (match (apply string (sort short-bools char-ci<=?))
                         ((? (compose (cut > <> 0) string-length) sbs)
                          (string-append "[-" sbs "]"))
                         (_ ""))
                       (map (cut string-join <>
                                 (string-append "\n" (whitespace)))
                            (map (cut sort <> string-ci<=?)
                                 (list long-bools short-rest long-rest)))))))
            ;; Resulst in:
            ;; (list (list of chars) (formated long opts)
            ;;       (formatted short rest opts) (formatted long rest opts))
            (fold (lambda (opt sorted)
                    (match sorted
                      ((short-bools long-bools short-rest long-rest)
                       (match opt
                         ((or ($ <puboption> n v t _ s _ _ e)
                              ($ <openoption> n v t _ _ s _ _ e))
                          (cond ((and s (boolproc? t)) ; short bool
                                 (list (cons s short-bools) long-bools
                                       short-rest long-rest))
                                ((boolproc? t) ; long bools
                                 (list short-bools
                                       (cons (string-append "[--"
                                                            (symbol->string n)
                                                            "]")
                                             long-bools)
                                       short-rest
                                       long-rest))
                                (s
                                 (list short-bools
                                       long-bools
                                       (cons (string-append "[-"
                                                            (string s)
                                                            " " e "]")
                                             short-rest)
                                       long-rest))
                                (else
                                 (list short-bools
                                       long-bools
                                       short-rest
                                       (cons (string-append "[--"
                                                            (symbol->string n)
                                                            "=" e "]")
                                             long-rest)))))
                         (_ (throw 'config "Should not have happened"))))))
                  '(() () () ())
                  opts)))
   (string-append "\n" (whitespace))))

(define configuration-write
  (io-lift
   (lambda (configuration output-port)
     "Output is unspecified. Write CONFIGURATION to OUTPUT-PORT."
     ((configuration-parser-writer
       (configuration-parser configuration)) configuration output-port))
   'output))

(define configuration-read
  (io-lift
   (lambda (configuration input-port)
     "Output is a merged configuration.  Merge configuration from INPUT-PORT."
     ((configuration-parser-reader
       (configuration-parser configuration)) configuration input-port))
   'input))

;;; config ends here
