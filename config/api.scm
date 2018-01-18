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
            parser-file parser-read parser-write parser-write-complete

            identity-parser

            <secret>
            secret secret?
            secret-name secret-default secret-synopsis

            <switch>
            switch switch?
            switch-name switch-default switch-test switch-handler
            switch-character switch-synopsis switch-description switch-example
            switch-optional?

            <setting>
            setting setting?
            setting-name setting-default setting-test setting-handler
            setting-character setting-synopsis setting-description
            setting-example setting-optional?

            <argument>
            argument argument?
            argument-name argument-default argument-test argument-test
            argument-handler argument-synopsis argument-description
            argument-example argument-optional?

            <configuration>
            configuration configuration?
            configuration-name configuration-synopsis
            configuration-description configuration-keywords
            configuration-arguments configuration-subcommands
            configuration-directory configuration-version
            configuration-license configuration-copyright configuration-author
            configuration-parser configuration-alias
            configuration-generate-help? configuration-generate-usage?
            configuration-generate-version? configuration-generate-cmdtree?
            configuration-wanted

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
            features-alias features-subcommands

            <metadata>
            metadata metadata?
            metadata-directory metadata-version metadata-license
            metadata-copyright metadata-author metadata-parser
            metadata-generate-help? metadata-generate-usage?
            metadata-generate-version?

            <valus>
            valus valus?
            valus-keywords valus-arguments

            <path>
            path path?
            path-given path-eager?

            in-cwd in-home

            n-configuration-files? single-configuration-file?

            keyword-name
            keyword-handler
            keyword-character
            keyword-default
            set-keyword-default
            set-argument-default
            inverted-next-config

            subcommand-reagents

            codex-metadatum codex-feature
            full-command
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
  (synopsis     secret-synopsis     (default "")))

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
  (optional?    switch-optional?    (default #t)))

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
  (optional?    setting-optional?    (default #f)))

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

(define (keyword-handler keyword)
  "Return KEYWORD handler or throw error."
  (match keyword
    (($ <secret>) (throw 'keyword-handler "SECRET's don't have handlers"))
    (($ <switch>) (switch-handler keyword))
    (($ <setting>) (setting-handler keyword))
    (n (throw 'keyword-handler "no matching pattern" n))))

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
  (optional?    argument-optional?    (default #t)))

(define (set-argument-default arg value)
  "Return a new version of argument ARG, with its default replaced by VALUE."
  (argument (inherit arg) (default value)))

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
  (wanted            configuration-wanted            (default '()))
  (keywords          configuration-keywords          (default '()))
  (arguments         configuration-arguments         (default '()))
  (subcommands       configuration-subcommands       (default '()))
  ;; Configuration-directory ::= <path> || (<path> ...)
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
  (generate-cmdtree? configuration-generate-cmdtree? (default #t)))

(define-record-type <empty-configuration>
  (empty-configuration) empty-configuration?)

(define (configuration-tuple configuration)
  "Return an assoc entry of CONFIGURATION name and CONFIGURATION
(i.e. '($config-name . $config))"
  (cons (configuration-name configuration) configuration))

(define-record-type* <path>
  path make-path
  path?
  (given  path-given)
  (eager? path-eager? (default #t)))

(define* (in-cwd #:optional (location "") eager?)
  (path (given (string-append (getcwd) file-name-separator-string location))
        (eager? eager?)))

(define* (in-home location #:optional wait?)
  (path (given (string-append (getenv "HOME") file-name-separator-string
                              location))
        (eager? (not wait?))))

(define (n-configuration-files? conf-dir)
  (list? conf-dir))

(define (single-configuration-file? conf-dir)
  (negate n-configuration-files?))

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
;; - fields that will be inherited, if defined, are 'metadata':
;;   + directory
;;   + version
;;   + license
;;   + copyright
;;   + author
;;   + parser
;;   + generate-help?
;;   + generate-usage?
;;   + generate-version?
;; - keywords & arguments are 'valus', they are inherited if wanted:
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

(set-record-type-printer!
 <codex>
 (lambda (codex port)
   (format port "#<codex features metadata valus reagents>")))

(define-record-type <reagents>
  (reagents inverted commandline raw configuration)
  reagents?
  (inverted      reagents-inverted)
  (commandline   reagents-commandline)
  (raw           reagents-raw)
  (configuration reagents-configuration))

(define-record-type <features>
  (features name synopsis description alias subcommands)
  features?
  (name         features-name)
  (synopsis     features-synopsis)
  (description  features-description)
  (alias        features-alias)
  (subcommands  features-subcommands))

(define-record-type <metadata>
  (metadata directory version license copyright author parser
            generate-help? generate-usage? generate-version?
            generate-cmdtree?)
  metadata?
  (directory         metadata-directory)
  (version           metadata-version)
  (license           metadata-license)
  (copyright         metadata-copyright)
  (author            metadata-author)
  (parser            metadata-parser)
  (generate-help?    metadata-generate-help?)
  (generate-usage?   metadata-generate-usage?)
  (generate-version? metadata-generate-version?)
  (generate-cmdtree? metadata-generate-cmdtree?))

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

(define (inverted-next-config inverted)
  "Return the configuration of the next entry in the list of INVERTED."
  (cdar inverted))

;;;; Subcommand handlers

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
  ;; strip script name from commandline
  (let ((base (glue configuration #f "" (cdr commandline) '())))
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

(define (full-command codex)
  "Return the breadcrumb to the subcommand in CODEX as list form."
  (reverse (map (compose symbol->string car)
                (reagents-inverted (codex-reagents codex)))))

(define (codex-feature key codex)
  "Return the feature identified by KEY."
  (let ((features (codex-features codex)))
    (match key
      ('name (features-name features))
      ('synopsis (features-synopsis features))
      ('description (features-description features))
      ('alias (features-alias features))
      ('subcommands (features-subcommands features))
      (n (throw 'codex-feature "no matching pattern" n)))))

(define (codex-metadatum key codex)
  "Return the metadatum identified by KEY."
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
      ('generate-cmdtree? (metadata-generate-cmdtree? metadata))
      (n (throw 'codex-metadatum "no matching pattern" n)))))

(define (find-argument key arguments)
  "Return the argument identified by KEY in ARGUMENTS or #f."
  (catch 'found
    (lambda ()
      (fold (lambda (candidate result)
              (match candidate
                ((? (compose (cut eq? key <>) argument-name) jackpot)
                 (throw 'found (argument-default jackpot)))
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
  (make-parser file-proc read-proc write-proc write-complete-proc)
  parser?
  (file-proc parser-file-proc)
  (read-proc parser-read-proc)
  (write-proc parser-write-proc)
  (write-complete-proc parser-write-complete-proc))

(define (parser-file parser <path> subcmd-name)
  ((parser-file-proc parser) <path> subcmd-name))

(define (parser-write parser file-path subcmd-name subcmd-desc subcmd-synopsis
                      . settings)
  (apply (parser-write-proc parser) file-path subcmd-name subcmd-desc
         subcmd-synopsis settings))

(define (parser-read parser file-path)
  ((parser-read-proc parser) file-path))

(define (parser-write-complete configuration)
  (match (parser-write-complete-proc (configuration-parser configuration))
    (#f #f)
    ((? procedure? proc) (proc configuration))))


;;;; Parsers


;;;;; Identity Parser

;; This parser is the identity parser: it creates no files and simply returns
;; the natural codex for its passed reagents.
;;
;; The result of using the identity parser is that settings are reduced to
;; switches (as no configuration files are available to set defaults)

(define identity-parser
  (make-parser (const "") (const '()) (const #t) (const #f)))
