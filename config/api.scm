;;; Config --- Configuration specification in GNU Guile
;;; Copyright © 2017 Alex Sassmannshausen <alex@pompo.co>
;;;
;;; This file is part of Guile-Config.
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
;;; along with Guile-Config; if not, contact:
;;;
;;; Free Software Foundation           Voice:  +1-617-542-5942
;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (config api)
  #:use-module (config licenses)
  #:use-module (config records)
  #:use-module (config helpers)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (
            <empty>
            empty empty?

            <parser>
            make-parser parser?
            parser-reader parser-writer

            simple-parser identity-parser

            <secret>
            secret secret?
            secret-name secret-default secret-synopsis secret-inheritable?

            <switch>
            switch switch?
            switch-name switch-default switch-test switch-handler
            switch-character switch-synopsis switch-description switch-example
            switch-optional? switch-inheritable?

            <setting>
            setting setting?
            setting-name setting-default setting-test setting-handler
            setting-character setting-synopsis setting-description
            setting-example setting-optional? setting-inheritable?

            <argument>
            argument argument?
            argument-name argument-default argument-test argument-test
            argument-handler argument-synopsis argument-description
            argument-example argument-optional? argument-inheritable?

            <configuration>
            configuration configuration?
            configuration-name configuration-synopsis
            configuration-description configuration-keywords
            configuration-arguments configuration-subcommands
            configuration-directory configuration-version
            configuration-license configuration-copyright configuration-author
            configuration-parser configuration-alias
            configuration-generate-help? configuration-generate-usage?
            configuration-generate-version? configuration-inheritance?

            <reagents>
            reagents reagents?
            reagents-inverted reagents-commandline reagents-raw
            reagents-configuration

            <codex>
            codex codex?
            codex-features codex-metadata codex-valus codex-reagents
            set-codex-valus

            <features>
            features features?
            features-name features-synopsis features-description
            features-alias features-subcommands features-inheritance?

            <metadata>
            metadata metadata?
            metadata-directory metadata-version metadata-license
            metadata-copyright metadata-author metadata-parser
            metadata-generate-help? metadata-generate-usage?
            metadata-generate-version?

            <valus>
            valus valus?
            valus-keywords valus-arguments

            keyword-name
            keyword-character
            keyword-default
            set-keyword-default
            inheritable?
            inverted-next-name inverted-next-config

            subcommand-reagents
            subcommand-path

            codex-metadatum codex-feature
            find-keyword find-argument
            ))


;;;; Empty Flags

;; This value is what inheritable settings default to when not initiated.  It
;; allows us to determine whether inheritance should take place.

(define-record-type <empty>
  (empty)
  empty?)


;;;; Secret, Switches & Settings

;; Secret, Switches & Settings are settings configured for a program's
;; execution.

;;;;; Secret

;; Secrets are internal to the program just set by the program's author.
(define-record-type* <secret>
  secret make-secret
  secret?
  (name         secret-name         (default 'secret))
  (default      secret-default      (default (empty)))
  (synopsis     secret-synopsis     (default ""))
  (inheritable? secret-inheritable? (default #t)))

;;;;; Switches & Settings

;; Switches and settings are keyword arguments to program invocations. e.g.:
;; guix package --upgrade=test -n
;;               ^--------------^------switch or setting

;;;;;; Switches

;; A switch can only be set on the command line
(define-record-type* <switch>
  switch make-switch
  switch?
  (name         switch-name)
  (default      switch-default      (default (empty)))
  (test         switch-test         (default string?))
  (handler      switch-handler      (default identity))
  (character    switch-character    (default #t))
  (synopsis     switch-synopsis     (default ""))
  (description  switch-description  (default ""))
  (example      switch-example      (default ""))
  (optional?    switch-optional?    (default #t))
  (inheritable? switch-inheritable? (default #t)))

;;;;;; Settings

;; Settings can be set in a configuration file, but overriden on the
;; commandline.
(define-record-type* <setting>
  setting make-setting
  setting?
  (name         setting-name)
  (default      setting-default      (default (empty)))
  (test         setting-test         (default string?))
  (handler      setting-handler      (default identity))
  (character    setting-character    (default #t))
  (synopsis     setting-synopsis     (default ""))
  (description  setting-description  (default ""))
  (example      setting-example      (default ""))
  (optional?    setting-optional?    (default #f))
  (inheritable? setting-inheritable? (default #t)))

;;;;; Secret, Switch & Setting Helpers: The Keyword Abstraction

;; Keywords can name any of the above three runtime configuration settings.

(define (keyword-character keyword)
  "Return the KEYWORD short version, i.e. character.  If character is true,
try to deduce from the KEYWORD name.  Else return the character setting."
  (match keyword
    (($ <secret>) #f)
    (($ <switch> name _ _ _ #t)
     (string-ref (symbol->string name) 0))
    (($ <switch> _ _ _ _ #f) #f)
    (($ <switch> _ _ _ _ character) character)
    (($ <setting> name _ _ _ #t)
     (string-ref (symbol->string name) 0))
    (($ <setting> _ _ _ _ #f) #f)
    (($ <setting> _ _ _ _ character) character)
    (n (throw 'keyword-character "no matching pattern" n))))

(define (keyword-name keyword)
  "Return KEYWORD name."
  (match keyword
    (($ <secret> name) name)
    (($ <switch> name) name)
    (($ <setting> name) name)
    (n (throw 'keyword-name "no matching pattern" n))))

(define (keyword-default keyword)
  "Return KEYWORD default."
  (match keyword
    (($ <secret> _ value) value)
    (($ <switch> _ value) value)
    (($ <setting> _ value) value)
    (n (throw 'keyword-default "no matching pattern" n))))

(define (set-keyword-default keyword value)
  "Return a new version of KEYWORD, with its default replaced by VALUE."
  (match keyword
    (($ <secret>) (secret (inherit keyword) (default value)))
    (($ <switch>) (switch (inherit keyword) (default value)))
    (($ <setting>) (setting (inherit keyword) (default value)))
    (n (throw 'set-keyword-default "no matching pattern" n))))

(define (keyword-inheritable? keyword)
  "Return KEYWORD inheritable? switch."
  (match keyword
    (($ <secret>) (secret-inheritable? keyword))
    (($ <switch>) (switch-inheritable? keyword))
    (($ <setting>) (setting-inheritable? keyword))
    (n (throw 'keyword-inheritable? "no matching pattern" n))))

;;;; Arguments

;; Arguments are positional arguments to command line invocations, e.g.
;; cat /path/to/file
;;     ^---- argument

(define-record-type* <argument>
  argument make-argument
  argument?
  (name         argument-name)
  (default      argument-default      (default (empty)))
  (test         argument-test         (default string?))
  (handler      argument-handler      (default identity))
  (synopsis     argument-synopsis     (default ""))
  (description  argument-description  (default ""))
  (example      argument-example      (default ""))
  (optional?    argument-optional?    (default #t))
  (inheritable? argument-inheritable? (default #t)))

(define (inheritable? obj)
  "Return #t if OBJ is inheritable."
  (match obj
    (($ <secret>) (secret-inheritable? obj))
    (($ <switch>) (switch-inheritable? obj))
    (($ <setting>) (setting-inheritable? obj))
    (($ <argument>) (argument-inheritable? obj))
    (n (throw 'inheritable? "no matching pattern" n))))

;;;; Configurations

;; A configuration is the object declared by a program author to describe a
;; program's runtime configuration domain.

;; When declaring a program's configuration options, this is normally the
;; starting point.

;; A program can have nested sub-commands, e.g.
;; guix package --help
;;      ^---- sub-command
;; They are captured in configuration objects through the `subcommands' field,
;; which contains a list of `sub-configurations', themselves configuration
;; objects.

(define-record-type* <configuration>
  configuration make-configuration
  configuration?
  (name              configuration-name)
  (synopsis          configuration-synopsis          (default ""))
  (description       configuration-description       (default ""))
  (keywords          configuration-keywords          (default '()))
  (arguments         configuration-arguments         (default '()))
  (subcommands       configuration-subcommands       (default '()))
  (directory         configuration-directory         (default (empty)))
  (version           configuration-version           (default (empty)))
  (license           configuration-license           (default (empty)))
  (copyright         configuration-copyright         (default (empty)))
  (author            configuration-author            (default (empty)))
  (parser            configuration-parser            (default (empty)))
  (alias             configuration-alias             (default #f))
  (generate-help?    configuration-generate-help?    (default #t))
  (generate-usage?   configuration-generate-usage?   (default #t))
  (generate-version? configuration-generate-version? (default #t))
  (inheritance?      configuration-inheritance?      (default #t)))

(define-record-type <empty-configuration>
  (empty-configuration) empty-configuration?)

(define (configuration-tuple configuration)
  "Return an assoc entry of CONFIGURATION name and CONFIGURATION
(i.e. '($config-name . $config))"
  (cons (configuration-name configuration) configuration))

;;;; Internal API

;;;;; Codex

;; A codex is the object that a configuration spec is compiled to once
;; inheritance of the various fields in a configuration spec is resolved.
;;
;; Inheritance follows strict rules:
;; - fields that cannot be inherited are 'features':
;;   + name
;;   + synopsis
;;   + description
;;   + alias
;;   + subcommands
;;   + inheritance?
;; - fields that can be inherited are 'metadata':
;;   + directory
;;   + version
;;   + license
;;   + copyright
;;   + author
;;   + parser
;;   + generate-help?
;;   + generate-usage?
;;   + generate-version?
;; - keywords & arguments are 'valus':
;;   + keywords
;;   + arguments
;; - reagents are data for optimization & documentation purposes:
;;   + inverted configuration
;;   + modified commandline
;;   + raw commandline
;;
;; Valus are special, because these are the fields that are not only
;; inheritable, but also potentially overridden by the end-user on the
;; commandline or in configuration files.

(define-immutable-record-type <codex>
  (codex features metadata valus reagents)
  codex?
  (features codex-features)
  (metadata codex-metadata)
  (valus    codex-valus set-codex-valus)
  (reagents codex-reagents))

(define-record-type <reagents>
  (reagents inverted commandline raw configuration)
  reagents?
  (inverted      reagents-inverted)
  (commandline   reagents-commandline)
  (raw           reagents-raw)
  (configuration reagents-configuration))

(define-record-type <features>
  (features name synopsis description alias subcommands inheritance?)
  features?
  (name         features-name)
  (synopsis     features-synopsis)
  (description  features-description)
  (alias        features-alias)
  (subcommands  features-subcommands)
  (inheritance? features-inheritance?))

(define-record-type <metadata>
  (metadata directory version license copyright author parser
            generate-help? generate-usage? generate-version?)
  metadata?
  (directory         metadata-directory)
  (version           metadata-version)
  (license           metadata-license)
  (copyright         metadata-copyright)
  (author            metadata-author)
  (parser            metadata-parser)
  (generate-help?    metadata-generate-help?)
  (generate-usage?   metadata-generate-usage?)
  (generate-version? metadata-generate-version?))

(define-record-type <valus>
  (valus keywords arguments)
  valu?
  (keywords  valus-keywords)
  (arguments valus-arguments))

;;;;; Inverted

;; Inverted is a list with as its first element the lowest requested
;; subcommand/configuration.  The next element is its direct ancestor, and so
;; on.
;; The last element will always be the root configuration.
;; This datastructure makes inheritance resolution trivial as we simply walk
;; the list, following the inheritance protocol.

(define (inverted-next-name inverted)
  "Return the name of the next entry in the list of INVERTED."
  (caar inverted))

(define (inverted-next-config inverted)
  "Return the configuration of the next entry in the list of INVERTED."
  (cdar inverted))

;;;; Subcommand handlers

(define (subcommand-inverted commandline configuration)
  "Return a list of tuples, consisting of (name . configuration), starting
with the lowest subcommand in CONFIGURATION selected by COMMANDLINE."
  ;; This procedure simplifies inheritance walks dramatically, as we no longer
  ;; have to walk the trees.  We simply check each field in the first
  ;; configuration, determine which need inheritance, and then take that
  ;; inherited value from the next highest available configuration.
  (subcommand-parse (lambda (current next name rest inverted)
                      (cons (configuration-tuple (or next current))
                            inverted))
                    commandline
                    configuration))

(define (subcommand-path commandline configuration)
  "Return a breadcrumb trail for the subcommands in COMMANDLINE pointing the
way through CONFIGURATION, to the lowest subcommand."
  (reverse
   (subcommand-parse (lambda (current next name rest path)
                       (cons (configuration-name (or next current))
                             path))
                     commandline
                     configuration)))

(define (subcommand-reagents commandline configuration)
  "Return the reagents object requested by COMMANDLINE over CONFIGURATION.

Most importantly, the reagents contains the inverted for the subcommand
requested in COMMANDLINE

No Inheritance resolution takes place here yet; this is merely a preparatory
stage."
  (match (subcommand-parse
          (lambda (current next name rest result)
            (cons (cons (configuration-tuple (or next current))
                        (match result
                          ((inverted . newcommandline) inverted)
                          (() '())))
                  rest))
          commandline
          configuration)
    ((inverted . newcommandline)
     ;; Return reagents for the inverted subcommand requested on COMMANDLINE.
     (reagents inverted newcommandline commandline configuration))
    (n (throw 'subcommand-reagents "no matching pattern" n))))

(define* (subcommand-parse glue commandline configuration)
  "GLUE should be a procedure of 5 arguments: it will be passed the current
configuration, the next configuration, the current name in the commandline,
the rest of the commandline and the result thus far.

COMMANDLINE is the commandline as passed to the program.

CONFIGURATION should be a <configuration>."
  (let ((base (glue configuration #f "" commandline '())))
    (match commandline
      ((scriptname . ()) base)
      ((scriptname . (name . rest))
       (let lp ((name name)
                (rest rest)
                (current configuration)
                (path base))
         (match (find-subcommand name current)
           ((? configuration? next)
            (if (null? rest)
                (glue current next name rest path)
                (lp (first rest)
                    (cdr rest)
                    next
                    (glue current next name rest path))))
           (_ path)
           (n (throw 'subcommand-parse "no matching pattern" n)))))
      (n (throw 'subcommand-parse "no matching pattern" n)))))

(define (find-subcommand name configuration)
  "Return the subcommand in CONFIGURATION identified by NAME, or #f."
  (match (filter-map (lambda (candidate)
                       (if (or (eq? (string->symbol name)
                                    (configuration-name candidate))
                               (eq? (string->symbol name)
                                    (configuration-alias candidate)))
                           candidate
                           #f))
                     (configuration-subcommands configuration))
    (() #f)
    ((config) config)
    (_ (throw 'find-subcommand "Should not have happened."))))

(define (codex-feature key codex)
  "Return the feature identified by KEY."
  (let ((features (codex-features codex)))
    (match key
      ('name (features-name features))
      ('synopsis (features-synopsis features))
      ('description (features-description features))
      ('alias (features-alias features))
      ('subcommands (features-subcommands features))
      ('inheritance? (features-inheritance? features))
      (n (throw 'codex-feature "no matching pattern" n)))))

(define (codex-metadatum key codex)
  "Return the metadatum identified by KEY."
  (force
   (let ((metadata (codex-metadata codex)))
     (match key
       ('directory (metadata-directory metadata))
       ('version (metadata-version metadata))
       ('license (metadata-license metadata))
       ('copyright (metadata-copyright metadata))
       ('author (metadata-author metadata))
       ('parser (metadata-parser metadata))
       ('generate-help? (metadata-generate-help? metadata))
       ('generate-usage? (metadata-generate-usage? metadata))
       ('generate-version? (metadata-generate-version? metadata))
       (n (throw 'codex-metadatum "no matching pattern" n))))))

(define (find-argument key arguments)
  "Return the argument identified by KEY in ARGUMENTS or #f."
  (catch 'found
    (lambda ()
      (fold (lambda (candidate result)
              (match candidate
                ((? (compose (cut eq? key <>) argument-name) jackpot)
                 (throw 'found jackpot))
                (_ result)))
            #f
            arguments))
    (lambda (k v) v)))

(define (find-keyword key keywords)
  "Return the keyword identified by KEY in KEYWORDS or #f."
  (catch 'found
    (lambda ()
      (fold (lambda (candidate result)
              (match candidate
                ((? (compose (cut eq? key <>) keyword-name) jackpot)
                 (throw 'found (keyword-default jackpot)))
                (_ result)))
            #f
            keywords))
    (lambda (k v) v)))



;;;; Configuration Parsers
;;;
;;; Configuration parsers are simple records consisting of a "reader" and a
;;; "writer".  Each of these should be procedures of 2 arguments, a
;;; <configuration> and a port.  The writer should emit the options of
;;; <configuration> to port.  It's return is ignored.  The reader should merge
;;; the contents of port with <configuration> and return that merged
;;; <configuration>.

(define-record-type <parser>
  (make-parser reader writer)
  parser?
  (reader parser-reader)
  (writer parser-writer))


;;;; Parsers


;;;;; Identity Parser

;; This parser is the identity parser: it creates no files and simply returns
;; the natural codex for its passed reagents.
;;
;; The result of using the identity parser is that settings are reduced to
;; switches (as no configuration files are available to set defaults)

(define identity-parser
  (make-parser (lambda (codex reagents) codex)
               (lambda (configuration reagents)
                 'fake-side-effect)))

;;;;; The Simple Parser

;; This parser is as simple and naive as can be:
;; - For each (sub-)command it generates one configuration file
;; - Settings are written out as simple scheme objects
;; - The reader simple traverses all configuration files and loads the scheme
;;   objects 

;; FIXME: currently reduced to identity parser
(define simple-parser
  (make-parser
   ;; Reader
   (lambda (configuration port)
     '(set-configuration-options
       configuration
       (map ((lambda (opts)
               (lambda (opt)
                 (match opt
                   ((name . (? open-option? opt))
                    (let ((new (assq-ref opts name)))
                      (if ((openoption-test opt) new)
                          (cons name (set-openoption-value opt new))
                          (begin
                            (format (current-error-port)
                                    "Configuration predicate failed: ~a [~s]~%"
                                    name new)
                            (exit 1)))))
                   (_ opt))))
             (stream->list (port->stream port read)))
            (configuration-options configuration)))
     configuration)
   ;; Writer
   (lambda (configuration port)
     '(match configuration
        (($ <configuration> name _ opts _ _ terse long _)
         (format port ";;;; ~a - ~a~%" name terse)
         (when long
           (for-each (lambda (line)
                       (pretty-print line port #:per-line-prefix ";;; "
                                     #:display? #t))
                     (cons "" (string-split (fill-paragraph long 68)
                                            #\newline))))
         (format port "~%")
         (for-each (lambda (value)
                     (match value
                       ((name . ($ <openoption> name value _ _ _ terse long))
                        (format port ";;;; ~a~%;;;~%" name)
                        (for-each (lambda (line)
                                    (pretty-print line port
                                                  #:per-line-prefix ";;; "
                                                  #:display? #t))
                                  (string-split (fill-paragraph terse 68)
                                                #\newline))
                        (when long
                          (for-each
                           (lambda (line)
                             (pretty-print line port
                                           #:per-line-prefix ";;; "
                                           #:display? #t))
                           (cons "" (string-split (fill-paragraph long 68)
                                                  #\newline))))

                        (format port "~%")
                        (pretty-print (cons name value) port #:width 72
                                      #:max-expr-width 72)
                        (format port "~%"))
                       (_ #f)))
                   (sort opts
                         (lambda (a b)
                           (string-ci<? (symbol->string (car a))
                                        (symbol->string (car b)))))))
        (n (throw 'simple-parser "no matching pattern" n)))
     configuration)))
