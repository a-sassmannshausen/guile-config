;; config.scm --- The config module    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 12 July 2015
;;
;; This file is part of Guile-Config.
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
  #:re-export  (private-option public-option open-option configuration-parser
                               configuration-print getopt-print)
  #:export     (
                getopt-config
                getopt-config-auto
                getmio-config
                getmio-config-auto
                option-ref
                subcommand
                make-help-emitter
                make-version-emitter
                configuration
                ))


;;;; Porcelain

(define (getmio-config args configuration)
  "Return a monadic IO value, which, when evaluated, returns the
<configuration> derived from merging CONFIGURATION, ARGS and CONFIGURATION's
configuration files."
  (mlet* %io-monad
      ((ignore (ensure-config-files configuration))
       (subcommands -> (establish-subcommands configuration args))
       (merged-config (merge-config-file-values configuration subcommands)))
    ;; derive/merge-config-getopt is non-monadic, so we wrap in return!
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

(define* (option-ref getopt key #:optional default)
  "Return the value for KEY in CONFIGURATION, or DEFAULT if it cannot be
found."
  (if (null? key)
      (getopt-free-params getopt)
      (match (assq key (configuration-options (getopt-configuration getopt)))
        (#f default)
        ((k . v) (match v
                   (($ <puboption> n v) v)
                   (($ <prioption> n v) v)
                   (($ <openoption> n v) v))))))

(define (subcommand getopt)
  "Return the currently active subcommand of <configuration> CONFIGURATION.

This is an alias to `configuration-name', as, in the case of an active
subcommand, we reduce the root configuration to that subcommand's
configuration.  Hence the name of that configuration will be the name of the
subcommand currently active."
  (configuration-name (getopt-configuration getopt)))

(define* (make-help-emitter getopt #:optional (port #t))
  "Traverse the config in GETOPT, building a GNU-style help message as we do
so and emit it to PORT."
  (define (filter-opts opts)
    (filter (lambda (x) (or (public-option? x) (open-option? x))) opts))
  (let ((config (getopt-configuration getopt)))
    (match (configuration-options config)
      (((names . (? option? opts)) ...)
       ;; Short Help
       (format port "Usage: ~a ~a~%"
               (string-append (symbol->string (configuration-name config))
                              (match (configuration-alias config)
                                (#f "")
                                (alias
                                 (string-append " | "
                                                (symbol->string alias)))))
               (sort-opts (filter-opts opts)
                          (+ (string-length "Usage: ")
                             (string-length (symbol->string
                                             (configuration-name config)))
                             (match (configuration-alias config)
                               (#f 0)
                               (alias
                                (+ (string-length " | ")
                                   (string-length (symbol->string alias)))))
                             1)))
       ;; Detailed Help
       (format port "~%Options:~%~a~%"
               (sort-detailed-opts (filter-opts opts))))
      (_ (throw 'config "CONFIG is invalid.")))
    ;; Subcommand listing
    (match (sort-subcommands (configuration-configs config))
      ("" #f)
      (subcommands (format port "~%Subcommands:~%~a~%" subcommands)))
    ;; Synopsis
    (if (configuration-long config)
        (format port "~%~a~%"
                (fill-paragraph (configuration-long config) 80)))))

(define* (make-version-emitter getopt #:optional port)
  "Traverse the config in GETOPT, building a GNU-style version message as we
do so and emit it to PORT."
  (format port "~a~%~a~a~a~%"
          (match (list (configuration-name (getopt-configuration getopt))
                       (option-ref getopt 'version-number #f))
            ((name #f) (symbol->string name))
            ((name (? string? version))
             (string-append (symbol->string name) " " version))
            ((name (? number? version))
             (string-append (symbol->string name)
                            (number->string version))))
          (match (map (cut option-ref getopt <> #f) '(copyright author))
            ((or (#f _) (_ #f)) "")
            ((years author)
             (string-append "Copyright (C) "
                            (string-join (map number->string years) ", ")
                            " " author "\n")))
          (match (option-ref getopt 'license #f)
            ((? license? license)
             (string-append (license->string license) "\n"))
            (_ ""))
          "This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."))

;;; FIXME: We should have a way to parse non-keyword args  [Version 0.9.5?]
;;;        We should do this by:
;;; - adding additional field to configuration (argument-pattern) — which
;;;   should be a simple `match' spec.
;;; - adding additional logic to getopt parse, to match above field, and add
;;;   it to resulting values list
;;; - allow use of option-ref to access these
;;; - add logic to configuration-spec compiler, to check that keys in each
;;;   configuration, including from `argument spec' are unique (meaningful
;;;   error if not).
(define* (configuration name terse values #:key config-dir
           long help? usage? version? license copyright author
           (version-test? string?) (parser simple-parser) (alias #f))
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
SIMPLE-PARSER.
 - ALIAS: an additional name for this configuration, generally intended as a
shorter alternative to the full name."
  ;; If we have been provided with convenience option values, we should
  ;; augment our configuration-options before finally instantiating
  ;; <configuration>.
  (define (augment-if proc do? values)
    (match do?
      (#t (cons (proc) values))
      (#f values)
      (_ (match (proc do? version-test?)
           ((vrsion vrsion-num) (cons* vrsion vrsion-num values))))))
  (define* (augment-opts opts #:optional next)
    (define (version-augment opts)
      (list (if version?
                (match (complex-version version? version-test?)
                  ((vrsion vrsion-num)
                   (cons* vrsion vrsion-num opts))
                  (_ (throw 'config-spec
                            "This should really not have happened.")))
                opts)
            license-augment))
    (define (license-augment opts)
      (list (if license (license-maker license opts) opts)
            copyright-augment))
    (define (copyright-augment opts)
      (list (match copyright
              (#f opts)
              (((? integer? years) ...) (cons (copyright-maker years) opts))
              (_ (throw 'config-spec
                        "Invalid COPYRIGHT: should be a list of years.")))
            author-augment))
    (define (author-augment opts)
      (list (match author
              (#f opts)
              ((? string?) (cons (author-maker author) opts))
              (_ (throw 'config-spec
                        "Invalid AUTHOR: should be a string.")))
            #t))

    (match next
      ((? procedure?) (apply augment-opts (next opts)))
      (#f (apply augment-opts (version-augment opts)))
      (#t opts)))

  ;; We generate a <configuration> record, but only if we pass our basic
  ;; parsing of the values that were provided.
  (apply mecha-configuration
         `(,(check-name name)
           ,(match config-dir
              ;; We have a config file!
              ((and (? string?) (? absolute-file-name?)) config-dir)
              (#f
               ;; We have no config file, so only proceed if we have no values
               ;; to be written to it.
               (if (any open-option? values)
                   (throw 'config-spec
                          "Open options specified, but no config-dir!")
                   config-dir))
              ;; We've been given something odd for config-dir.
              (_ (throw 'config-spec
                        "CONFIG-DIR should be an absolute filepath, or #f.")))
           ;; Augment values and split it into opts & confs
           ,@(let loop ((values (augment-opts
                                 (augment-if usage usage?
                                             (augment-if help help? values))))
                        (opts '())
                        (confs '()))
               (match values
                 (()
                  (map reverse
                       ;; Generate final list of augmented opts and confs
                       (list opts confs)))
                 (((? option? opt) . rest)
                  (loop rest
                        (cons
                         (match opt
                           (($ <prioption> name) (cons name opt))
                           (($ <puboption> name) (cons name opt))
                           (($ <openoption> name) (cons name opt))) opts)
                        confs))
                 (((? configuration? conf) . rest)
                  (loop rest opts (cons (cons (configuration-name conf) conf)
                                        confs)))
                 (_ (throw 'config-spec
                                   "Invalid option in configuration."))))
           ,(match terse
              ((and (? string?) (? (compose (cut <= <> 40) string-length)))
               terse)
              (_ (throw 'config-spec
                        "TERSE should be a string of length less than 40.")))
           ,(match long
              ((or (? string?) #f) long)
              (_ (throw 'config-spec "LONG should be a string, or #f.")))
           ,(match parser
              ((? configuration-parser?) parser)
              (_ (throw 'config-spec
                        "PARSER should be a configuration parser.")))
           ,(match alias
              ((or (? symbol?) #f) alias)
              (_ (throw 'config-spec
                        "ALIAS should be a symbol, if specified."))))))


;;;; Plumbing

;; On the merge process:
;; We have
;; a) a list of subcommands, where the last element is the subcommand to
;;    finally executed, and each prior element is a breadcrumb on the trail to
;;    it.  The car is the next configuration on this breadcrumb trail.
;; b) configuration, which is the full configuration as defined by the app.
;;
;; [obsolete]
;; We want:
;; A new configuration which consists only of `subcommand's' configuration,
;; augmented by the default settings and config-file settings from parent
;; subcommands.
;; This means:
;; a) each setting that is not explicitly defined in `subcommand',
;;    but which exists in any parent, is added with the parent's default or
;;    config file value (all <configuration>s, except those in `subcommand'
;;    will simply be dropped).
;; b) each setting that is defined in `subcommand', and also in a parent
;;    command will use the value from the `subcommand' config or it's
;;    default.
;; c) We return this new configuration for parsing by
;;    `derive/merge-config...', so that any cli args passed that are valid for
;;    parent commands are valid for the child command.
;; [obsolete] reasoning:
;; This could be counter-intuitive.  I like this idea as it could
;; significantly shorten configuration definitions, but it should rely on an
;; 'inheritance' flag in configuration.  This flag could tell that all
;; lower level configurations should inherit from their 'inheritance #t'
;; parent.
;;
;;
;; Non-inheritance version:
;; A new configuration that simply reflects the configuration as defined by
;; `subcommand'.
;;
;; There are 3 separate operations here: locate a given configuration, read
;; that configuration's config file, merge the parent configuration into this
;; newly updated configuration if necessary.

;; => io read -> configuration.
(define (merge-config-file-values configuration subcommands)
  "Return a new configuration corresponding to the subcommand specified by the
breadcrumb trail in SUBCOMMANDS, and based on CONFIGURATION which has been
augmented by the configuration values from its configuration file."
  (define configuration-inheritance (const #f))
  (define (read-merge config)
    "Read the configuration file for CONFIG and return an augmented config in
the %io-monad."
    (if (and (configuration-file config)
             (any open-option? (configuration-options config)))
        (mlet* %io-monad
            ((old-io (set-io-input-file (configuration-file config)))
             (config (configuration-read config))
             (ignore (io-close-input-port old-io)))
          (return config))
        (with-monad %io-monad (return config))))
  (define (find-subconfiguration configuration config-name)
    "Return the subconfiguration in CONFIGURATION specified by CONFIG-NAME."
    (match (assq config-name (configuration-configs configuration))
      (#f (throw 'merge-config-file-values "Unknown subconfiguration: "
                 config-name))
      ((name . (? configuration? subconfiguration)) subconfiguration)
      (_ (throw 'merge-config-file-values "Not a <configuration>: "
                config-name))))

  (if (configuration-inheritance configuration)
      ;; Inherit workflow
      ;; For now guaranteed not implemented.
      ;; (recurse configuration subcommands)
      configuration
      ;; Simple workflow
      (match subcommands
        (() (read-merge configuration))
        ((first . rest)
         (merge-config-file-values (find-subconfiguration configuration first)
                                   rest)))))

;; => io write -> '()
(define (ensure-config-files configuration)
  "Check if CONFIGURATION's config-files exist (recursively), and create any
that do not exist.
Return values is unspecified in the io-monad."
  (define (ensure config)
    (if (or (not (configuration-file config))
            (file-exists? (configuration-file config))
            (any open-option?
                 (configuration-options configuration)))
        (with-monad %io-monad (return '()))
        (mlet* %io-monad
            ((ignore  (iomkdir-p (configuration-dir config)))
             (old-out (set-io-output-file (configuration-file config)))
             (ignore  (configuration-write config)))
          (io-close-output-port old-out))))
  (define (recurse values)
    (match values
      (() (with-monad %io-monad (return '())))
      (((name . (? configuration? config)) . rest)
       ;; create config, then recurse with (configuration-configs config) + rest
       (mlet* %io-monad
           ((ignore (ensure config))
            (ignore (recurse (configuration-configs config))))
         (recurse rest)))
      ((option . rest) (recurse rest))))

  (mlet* %io-monad
      ((ignore (ensure configuration)))
    (recurse (configuration-configs configuration))))


;;;; Helpers
(define (padded string longest)
  "Return STRING with white-space appended up to length LONGEST."
  (let moar ((name string)
             (padding (- longest (string-length string))))
    (if (> padding 0)
        (moar (string-append name " ") (1- padding))
        name)))

(define (sort-subcommands configs)
  "Return a formatted string consisting of the name and terse description of
the subcommands contained in CONFIGS."
  ;; Subcommand listing should be:
  ;; Subcommands:
  ;;   command1        command-terse
  ;;   command2        command-terse
  ;;   command3        command-terse
  ;; [2 spaces][padded name longest][ | padded alias longest-alias]terse
  (define (conf-spec name alias terse confs longest longest-alias)
    ;; result: '(((conf-name alias terse) ...) longest longest-alias)
    (list (cons (list name alias terse) confs)
          longest
          (if ((compose (cut > <> longest-alias) string-length) alias)
              (string-length alias)
              longest-alias)))
  (string-join
   (match (fold (lambda (conf result)
                  (match result
                    ((confs longest longest-alias)
                     (match conf
                       ((name . ($ <configuration> (= symbol->string n) _ _ _
                                                   t _ _ a))
                        (if ((compose (cut > <> longest) string-length) n)
                            (conf-spec n (if (symbol? a)
                                             (symbol->string a)
                                             "")
                                       t confs (string-length n)
                                       longest-alias)
                            (conf-spec n (if (symbol? a)
                                             (symbol->string a)
                                             "")
                                       t confs longest
                                       longest-alias)))))))
                '(() 0 0)
                configs)
     ((conf-specs longest longest-alias)
      (sort
       (map (match-lambda
              ((name alias terse)
               (string-append "  " (padded name longest)
                              (match alias
                                (#f (string-append "   "
                                                   (padded "" longest-alias)))
                                (a (string-append (if (string-null? a)
                                                      "   "
                                                      " | ")
                                                  (padded a longest-alias))))
                              "  "
                              terse)))
            conf-specs)
       string-ci<=?)))
   "\n"))

(define (sort-detailed-opts opts)
  "Return a formatted string of OPTS.  An example of our output:

    --name     -n   Name of user
    --target   -t   Target of game
    --zulu          Bogus option"
  (define (opt-spec name single-char terse opts longest)
    ;; result: `(((name single-char terse) ...) . longest)
    (cons (cons (list name single-char terse) opts)
          longest))
  (string-join
   (match (fold (lambda (opt result)
                  (match result
                    ((opts . longest)
                     (match opt
                       ((or ($ <puboption> (= symbol->string n) _ _ _ s t)
                            ($ <openoption> (= symbol->string n) _ _ _ s t))
                        (if ((compose (cut > <> longest) string-length) n)
                            (opt-spec n s t opts (string-length n))
                            (opt-spec n s t opts longest)))))))
                '(() . 0)
                opts)
     ((opt-specs . longest)
      (sort (map (match-lambda
                   ((n s t)
                    (string-append "  --" (padded n longest)
                                   (if s
                                       (string-append "  -" (string s))
                                       "    ")
                                   "  " t)))
                 opt-specs)
            string-ci<=?)))
   "\n"))

(define (sort-opts opts indent)
  "Return a formatted string of OPTS, INDENTED up to level INDENT.
This formatting is intended for the brief summary of our command."
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
                              ($ <openoption> n v t _ s _ _ e))
                          (let ((n (symbol->string n)))
                            (cond ((and s (boolproc? t)) ; short bool
                                   (list (cons s short-bools) long-bools
                                         short-rest long-rest))
                                  ((boolproc? t) ; long bools
                                   (list short-bools
                                         (cons (string-append "[--" n "]")
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
                                   `(,short-bools
                                     ,long-bools
                                     ,short-rest
                                     ,(cons (string-append "[--" n "=" e "]")
                                            long-rest))))))
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
