;; conf.scm --- The config module    -*- coding: utf-8 -*-
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

(define-module (conf)
  #:use-module (conf getopt)
  #:use-module (conf parser)
  #:use-module (conf spec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (conf monads)
  #:use-module (conf monads io)
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
                emit-help
                emit-version
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
    (return (derive/merge-config-getopt merged-config args subcommands
                                        configuration))))

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
                  (emit-help parsed port)
                  (exit 0))
                (when (option-ref parsed 'version #f)
                  (emit-version parsed port)
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
        ((name . option) (option-value option)))))

(define (subcommand getopt)
  "Return the currently active subcommand of <configuration> CONFIGURATION.

This is an alias to `configuration-name', as, in the case of an active
subcommand, we reduce the root configuration to that subcommand's
configuration.  Hence the name of that configuration will be the name of the
subcommand currently active."
  (configuration-name (getopt-configuration getopt)))

(define* (emit-help getopt #:optional (port #t))
  "Traverse the config in GETOPT, building a GNU-style help message as we do
so and emit it to PORT."
  (define (filter-opts opts)
    (filter (lambda (x) (or (public-option? x) (open-option? x))) opts))
  (let ((config (getopt-configuration getopt))
        (usage-string (string-append (_ "Usage") ": "))
        (options-string (string-append (_ "Options") ":"))
        (subcommands-string (string-append (_ "Subcommands") ":")))
    (match (configuration-options config)
      (((names . (? option? opts)) ...)
       ;; Short Help
       (let ((full-cmd-name (match (getopt-command-trail getopt)
                              ((root-cmd-name . ((subcmds aliases) ...))
                               (string-join (map symbol->string
                                                 (cons root-cmd-name
                                                       subcmds)))))))
         (format port "~a~a ~a~%" usage-string full-cmd-name
                 (sort-opts (filter-opts opts)
                            (+ (string-length usage-string)
                               (string-length full-cmd-name)
                               1))))
       ;; Detailed Help
       (format port "~%~a~%~a~%" options-string
               (sort-detailed-opts (filter-opts opts))))
      (_ (throw 'config (_ "CONFIG is invalid."))))
    ;; Subcommand listing
    (match (sort-subcommands (configuration-configs config))
      ("" #f)
      (subcommands (format port "~%~a~%~a~%" subcommands-string subcommands)))
    ;; Synopsis
    (if (configuration-long config)
        (format port "~%~a~%"
                (fill-paragraph (configuration-long config) 80)))))

(define* (emit-version getopt #:optional (port #t))
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
          (_ "This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.")))

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
(define* (configuration name terse values #:key (free-param '()) config-dir
           long help? usage? version? license copyright author
           (version-test? string?) (parser simple-parser) alias inherit)
  "Return a configuration.  NAME should be a symbol naming the configuration.
TERSE is a < 40 char description; VALUES is a list of config-options.  The
optional arguments:
 - FREE-PARAMS: a specification describing the type of free or positional
parameters which might be passed to this configuration.
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
shorter alternative to the full name.
 - INHERIT: If #t, inherit options and their values from parent
configurations.  Defaults to #f, and should only be used in subcommand
configurations.

The standard options, version, help and usage all inherit by default, when
inherit is set to #t."
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
                            (_ "This should really not have happened."))))
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
                        (_ "Invalid COPYRIGHT: should be a list of years."))))
            author-augment))
    (define (author-augment opts)
      (list (match author
              (#f opts)
              ((? string?) (cons (author-maker author) opts))
              (_ (throw 'config-spec
                        (_ "Invalid AUTHOR: should be a string."))))
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
                          (_ "Open options specified, but no config-dir!"))
                   config-dir))
              ;; We've been given something odd for config-dir.
              (_ (throw 'config-spec
                        (_ "CONFIG-DIR: absolute dirpatch, or #f."))))
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
                        (cons `(,(option-name opt) . ,opt) opts)
                        confs))
                 (((? configuration? conf) . rest)
                  (loop rest opts (cons (cons (configuration-name conf) conf)
                                        confs)))
                 (_ (throw 'config-spec
                           (_ "Invalid option in configuration.")))))
           ,(match free-params
              ((? list?) free-params)
              (_ (throw 'config-spec
                        (_ "FREE-PARAMS should be a list."))))
           ,(match terse
              ((and (? string?) (? (compose (cut <= <> 40) string-length)))
               terse)
              (_ (throw 'config-spec
                        (_ "TERSE should be a string shorter than 40."))))
           ,(match long
              ((or (? string?) #f) long)
              (_ (throw 'config-spec
                        (_ "LONG should be a string, or #f."))))
           ,(match parser
              ((? configuration-parser?) parser)
              (_ (throw 'config-spec
                        (_ "PARSER should be a configuration parser."))))
           ,(match alias
              ((or (? symbol?) #f) alias)
              (_ (throw 'config-spec
                        (_ "ALIAS should be a symbol, if specified."))))
           ,(match inherit
              ((or #t #f) inherit)
              (_ (throw 'config-spec
                        (_ "INHERIT should be #t or #f, if specified.")))))))


;;;; Plumbing

;; On the merge process:
;; We have
;; a) a list of SUBCOMMANDS, where the last element is the subcommand to
;;    finally execute, and each prior element is a breadcrumb on the trail to
;;    it.  The car is the next configuration on this breadcrumb trail.
;; b) CONFIGURATION, which is the full configuration as defined by the app.
;;
;; Inheritance:
;; Inheritance in this context means that a subcommand's configuration
;; inherits the *-options from its parent.
;; This inheritance is switched off by default, and can be switched on at:
;; - configuration level    [default: off]
;; - parent *-option level  [default: on]
;;
;; This means that:
;; - switching inheritance on at the 'configuration' level will result in
;;   all 'parent *-options' being inherited to this subcommand configuration,
;;   except for those that have been explicitly switched 'off' at the option
;;   level.
;; - inheritance at the '*-option level' can veto inheritance at the
;;   'configuration level': #t means inherit if subcommand config wants it; #f
;;   means force non-inheritance.
;;
;; There are 3 separate operations here:
;; 1) locate a given configuration,
;; 2) read that configuration's config file,
;; 3) merge the parent configuration into this newly updated configuration if
;;    inheritance is switched on
;;
;; First implementation implements just the per-configuration inheritance
;; flag.

;; => configuration subcommands -> io configuration.
(define (merge-config-file-values configuration subcommands)
  "Return a new configuration corresponding to the subcommand specified by the
breadcrumb trail in SUBCOMMANDS, and based on CONFIGURATION which has been
augmented by the configuration values from its configuration file."
  (let lp ((configuration configuration)
           (subcommands subcommands)
           (inheritance '()))

    ;;; This needs to be monadified!
    (match subcommands
      ;; We have reached the end
      ;; -> definitely merge in configuration file options, but if we have an
      ;; inheritance, descendants, rejoyce!
      (() (if (or (null? inheritance)
                  (not (configuration-inherit configuration)))
              ;; No subcommands and no inheritance, just read-merge!
              (read-merge configuration)
              (inheritance-merge configuration
                                 (inheritance-prune inheritance))))
      ;; We are still travelling to the final configuration
      ;; -> augment inheritance, advance to next configuration, and loop!
      (((config-name config-alias) . rest)
       (lp (find-subconfiguration configuration config-name)
           rest
           ;; The list of configurations that will need to be read-merged as
           ;; part of the inheritance
           (cons configuration inheritance))))))

;; => config -> io config
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

;; => configuration config-name -> configuration
(define (find-subconfiguration configuration config-name)
  "Return the subconfiguration in CONFIGURATION specified by CONFIG-NAME."
  (match (assq config-name (configuration-configs configuration))
    (#f (throw 'find-subconfiguration
               (_ "Unknown subconfiguration: ") config-name))
    ((name . (? configuration? subconfiguration)) subconfiguration)
    (_ (throw 'find-subconfiguration
              (_ "Not a <configuration>: ") config-name))))

;; => config inheritance -> io config
(define (inheritance-merge config inheritance)
  "Return CONFIG augmented by its configuration file and its INHERITANCE,
i.e. all it's inheritance granting parent configurations and their
configuration files.

INHERITANCE is a reversed list of configurations that we must read-merge
and continuously fold into our final configuration.

CONFIG is the simple configuration of the final subcommand we're executing,
i.e. the configuration we're merging into.

We only call this function if CONFIG wants its ancestral inheritage!"

  ;; We must:
  ;; 1 monadic map (reverse inheritance) with read-merge, returning the
  ;;   list of configuration file enhanced configs
  ;; 2 fold the resulting list into config obeying the following rules:
  ;;   + ignore all subconfigurations in the config we are merging; they
  ;;     are not relevant.
  ;;   + unless this is the final config, do not add config value if
  ;;     *-option inherit #f
  ;;   + if a config value does not exist, add it
  ;;   + if a config value exists, overwrite it with the new value

  (define* (merge-inheritance config merged-inheritance #:optional final)
    (define (merge name option merged)
      (cond ((assv name merged)
             ;; Exists in merged config -> must update config
             (fold (lambda (opt newly-merged)
                     (match opt
                       (((? (cut eqv? name <>)) . old-option)
                        (if (or final (option-inherit option))
                            (cons (cons name option) newly-merged)
                            ;; Option inherit is off and this is not final
                            ;; -> drop option.
                            newly-merged))
                       ;; Not our candidate, fold onwards.
                       (_ (cons opt newly-merged))))
                   '()
                   merged))
            ((or final (option-inherit option))
             ;; Does not exist -> we need to add it
             (cons `(,name . ,option) merged))
            (else
             ;; We don't want this option in merged.
             merged)))

    (let ((options (match config
                     (($ <configuration> name dir options)
                      (let lp ((options options)
                               (merged  (configuration-options
                                         merged-inheritance)))
                        (match options
                          (() merged)
                          (((name . opt) . rest)
                           (lp rest (merge name opt merged)))
                          (_ (throw 'merge-inheritance))))))))
      (if final
          (mecha-configuration (configuration-name config)
                               (configuration-dir config)
                               options
                               (configuration-configs config)
                               (configuration-free-params config)
                               (configuration-terse config)
                               (configuration-long config)
                               (configuration-parser config)
                               (configuration-alias config)
                               (configuration-inherit config))
          (set-configuration-options merged-inheritance options))))

  (mlet* %io-monad
      ((read-inheritance (mapm %io-monad read-merge inheritance))
       (merged-ancestors -> (fold merge-inheritance
                                  (empty-configuration)
                                  read-inheritance))
       (read-config      (read-merge config)))
    ;; Merge inheritance with our final configuration, ensuring it's final!
    (return (merge-inheritance read-config merged-ancestors #t))))

(define (inheritance-prune inheritance)
  "Return a list, a shortened and reversed version of inheritance, where all
configurations appearing after the first configuration that has inheritance
switched off, have been removed.

This is an optimization, eliminating configurations from having to be merged
when they will be dropped for inheritance purposes in any case."
  ;; We would like the inheritance list to only consist of those inheritances
  ;; that will not later be dropped because of a later configuration having
  ;; inherit #f.
  ;;
  ;; This can be achieved by folding inheritance as follows:
  ;; - list is received in reversed order, so direct parent of config comes
  ;;   first.
  ;; - fold ->
  ;;   + if parent inherit #f, drop all further ancestors
  ;;   + else, move further down the list
  ;;
  ;; This also reverses inheritance (as desired for inheritance merge)
  (cdr
   (fold (lambda (ancestor inheriting-ancestors)
           (match inheriting-ancestors
             ((#t . ancestors)
              (match ancestor
                ((? configuration-inherit)
                 `(#t . ,(cons ancestor ancestors)))
                (non-inheriting-ancestor
                 `(#f . ,(cons ancestor ancestors)))))
             (_ inheriting-ancestors)))
         '(#t . ())
         inheritance)))

;; => io write -> '()
(define (ensure-config-files configuration)
  "Check if CONFIGURATION's config-files exist (recursively), and create any
that do not exist.
Return values is unspecified in the io-monad."
  (define (ensure config)
    (with-monad %io-monad
      (munless (or (not (configuration-file config))
                   (any open-option?
                        (configuration-options configuration))
                   (file-exists? (configuration-file config)))
        (iomkdir-p (configuration-dir config))
        (mlet %io-monad
            ((old-out (set-io-output-file (configuration-file config))))
          (configuration-write config)
          (io-close-output-port old-out)))))
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
                                                   _ t _ _ a))
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
                         (_ (throw 'config
                                   (_ "Should not have happened")))))))
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

;;; conf.scm ends here
