;;; Config Spec --- Configuration specification in GNU Guile
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

(define-module (config config-spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (
            define-configuration
            configuration-print

            <prioption>
            private-option?
            prioption-name
            set-prioption-name
            prioption-value
            set-prioption-value
            prioption-test
            set-prioption-test
            prioption-terse
            set-prioption-terse
            prioption-long
            set-prioption-long
            define-private-option

            <puboption>
            public-option?
            puboption-name
            set-puboption-name
            puboption-value
            set-puboption-value
            puboption-test
            set-puboption-test
            puboption-single-char
            set-puboption-single-char
            puboption-terse
            set-puboption-terse
            puboption-long
            set-puboption-long
            define-public-option

            <openoption>
            open-option?
            openoption-name
            set-openoption-name
            openoption-value
            set-openoption-value
            openoption-conf-test
            set-openoption-conf-test
            openoption-test
            set-openoption-test
            openoption-single-char
            set-openoption-single-char
            openoption-terse
            set-openoption-terse
            openoption-long
            set-openoption-long
            define-open-option

            option?

            <configuration>
            configuration-name
            set-configuration-name
            configuration-dir
            set-configuration-dir
            configuration-values
            set-configuration-values
            configuration-terse
            set-configuration-terse
            configuration-long
            set-configuration-long
            define-configuration

            complex-version
            version
            version-number
            help
            usage
            ))

;;; Commentary:
;;;
;;;; Introduction
;;;
;;; Configuration specs provide a means to declare an applications
;;; configuration values.
;;;
;;; Those values are set privately (inside the application).
;;;
;;; Private values can be declared to be open — which means that they can be
;;; overriden at run time using command line arguments.
;;;
;;; Open values can be declared to be public: this means that they can
;;; also be overridden in configuration files.
;;;
;;;; Examples
;;;
;;;;; Private value
;;;
;;; An application generally declares a "version".  This version can
;;; be specified using a private value.  They could specify this in
;;; the following way:
;;; (define-private-option version
;;;   "This application's version string"
;;;   #:value "1.0"
;;;   #:test string?
;;;   #:long "This string is the canonical representation of the
;;;   version of our application")
;;;
;;; This specification compiles to an entry in the application's
;;; config record providing a "private-configuration-object" — the
;;; simplest configuration record, which just provides an interface to
;;; documentation and querying of the value, and automatic testing of
;;; the value when the configuration-record is created.
;;;
;;;;; Public value
;;;
;;; An application may want to implement the "--version" commandline
;;; option.  We will refer to the version private option value, and by
;;; default have this setting disabled.
;;; (define-public-option version-flag
;;;   "Emit this application's version string."
;;;   #:value #f
;;;   #:test boolean?
;;;   #:single-char #\v
;;;   #:long "Provide more detail about the version option.")
;;;
;;; This record compiles to an public-configuration-object.  In addition
;;; to the private features, this object also provides a feature to
;;; retrieve the getopt-long specification, allowing us to
;;; programatically work with it.
;;;
;;;;; Open value
;;;
;;; An application may wish to provide a default setting, which can be
;;; customized by the end-user in configuration files, as well as
;;; being overridden at run-time using commandline flags.
;;;
;;; For instance we may wish to provide a default log-level of 3,
;;; which the end-user can override in configuration files and each
;;; run by commandline specification.
;;; (define-open-option log-level
;;;  "Determines the verbosity of our log output."
;;;  #:value 3
;;;  #:test integer?
;;;  #:single-char #\l
;;;  #:long "Set this level to 1 for silence, up to 5 for extreme
;;;  verbosity.")
;;;
;;;  In this case we just need to guarantee that we:
;;;  a) set the default
;;;  b) parse the relevant configuration files to update the value
;;;  c) parse the commandline for a final update.
;;;  This procedure is guaranteed by the
;;;  "open-option-configuration-object", generated from the above
;;;  declaration.
;;;
;;;; Configurations
;;;
;;; Groups of options are collected in configurations.  A
;;; configuration defines:
;;; a) a (sub-)command name
;;; [b) a path to the configuration file directory]
;;; c) a list of option values and/or further configurations.
;;; d) terse docstring
;;; [e) long documentation string]
;;;
;;; The recursive nature of configurations allow us to specify
;;; sub-commands for applications.
;;;
;;;;; Example
;;;
;;; A program `foo' exposes 'log-level' and 'dry-run' options.  In
;;; addition it also exposes the `print' subcommand, which in turn
;;; exposes 'style' and 'user' options.
;;; An end user thus should be able to invoke:
;;; $ foo --log-level=4 --dry-run print --style=pretty --user=frob
;;;
;;; The program should create a configuration object containing the
;;; values for the parent (foo: log-level:4; dry-run:#t;) and then for
;;; the sub-command (print: style:pretty; user:frob).
;;;
;;; This would be defined as follows:
;;; (define-configuration "foo"
;;;  "Base configuration for the 'foo' program."
;;;  (list (define-configuration "bar"
;;;         "Configuration for the 'bar' subcommand."
;;;         #:config-dir "path/to/config/dir"
;;;         (list (define-open-option "style" ...)
;;;               (define-public-option "user" ...)))
;;;        (define-public-option "log-level" ...)
;;;        (define-public-option "dry-run" ...)))
;;;
;;; As the base configuration provides no open-options, it has no need
;;; for a configuration file: the #:config-dir option can be ommitted.
;;;
;;;;;
;;;
;;; In addition we can provide the optional keyword arguments
;;; '#:help', '#:version', '#:usage'.  These will automatically add
;;; GNU compliant help, version and usage commandline parameters to
;;; your application, and will allow you to extract IO-monadic
;;; snippets which can emit GNU compliant messages to the end-user.
;;;
;;; The '#:version' keyword expects an integer as an argument.  The
;;; two others simply expect #t/#f.
;;;
;;; Code:


;;;; Options

;;;;; Private Options
;;;
;;; Private options are specified in the program, but are not
;;; overridable by the end user, neither in configuration files, nor
;;; at the command line.
(define-immutable-record-type <prioption>
  (mecha-prioption name value test terse long)
  private-option?
  (name prioption-name set-prioption-name)
  (value prioption-value set-prioption-value)
  (test prioption-test set-prioption-test)
  (terse prioption-terse set-prioption-terse)
  (long prioption-long set-prioption-long))

(define* (define-private-option name terse #:key long
           (value '<unset>) (test boolean?))
  "Return a Private Option.  NAME should be a symbol naming the option and
TERSE should be a < 40 char decsription.
 - LONG: space for a longer description.
 - VALUE: the value assigned to this private option.
 - TEST: the predicate to check this VALUE against."
  (mecha-prioption (check-name name)
                   (check-value value)
                   (check-test test)
                   (check-terse terse)
                   (check-long long)))

;;;;; Public Options
;;;
;;; Public options are options that do not feature in configuration files, but
;;; which can be specified on the command line.
(define-immutable-record-type <puboption>
  (mecha-puboption name value test single-char terse long)
  public-option?
  (name puboption-name set-puboption-name)
  (value puboption-value set-puboption-value)
  (test puboption-test set-puboption-test)
  (single-char puboption-single-char set-puboption-single-char)
  (terse puboption-terse set-puboption-terse)
  (long puboption-long set-puboption-long))

(define* (define-public-option name terse #:key long
           (value '<unset>) single-char (test boolean?))
  "Return a Public Option.  NAME should be a symbol naming the option and
TERSE should be a < 40 char decsription.
 - LONG: space for a longer description.
 - VALUE: the value assigned to this private option.
 - SINGLE-CHAR: if set to a character, the single-char for the getopt-long
spec assigned to this option.
 - TEST: the predicate to check this VALUE against."
  (mecha-puboption (check-name name)
                   (check-value value)
                   (check-test test)
                   (check-single-char single-char)
                   (check-terse terse)
                   (check-long long)))

;;;;; Open Options
;;;
;;; Open options are options that have default values, which can be
;;; overridden in configuration files, and which can be overriden at
;;; the CLI.
(define-immutable-record-type <openoption>
  (mecha-openoption name value cli-test conf-test single-char terse long)
  open-option?
  (name openoption-name set-openoption-name)
  (value openoption-value set-openoption-value)
  (cli-test openoption-cli-test set-openoption-cli-test)
  (conf-test openoption-conf-test set-openoption-conf-test)
  (single-char openoption-single-char set-openoption-single-char)
  (terse openoption-terse set-openoption-terse)
  (long openoption-long set-openoption-long))

(define* (define-open-option name terse #:key single-char
           (value '<unset>) (cli-test boolean?) (conf-test boolean?) long)
  "Return a Public Option.  NAME should be a symbol naming the option and
TERSE should be a < 40 char decsription.
 - LONG: space for a longer description.
 - VALUE: the value assigned to this private option.
 - SINGLE-CHAR: if set to a character, the single-char for the getopt-long
spec assigned to this option.
 - CLI-TEST: the predicate to check this VALUE against, as a command-line
parameter.
 - CONF-TEST: the predicate to check this VALUE against, in configuration
files."
  (mecha-openoption (check-name name)
                    (check-value value)
                    (check-test cli-test)
                    (check-test conf-test)
                    (check-single-char single-char)
                    (check-terse terse)
                    (check-long long)))

;;;;; Generic Option Predicate

(define (option? obj)
  "Return #t if OBJ is an option; #f otherwise."
  (match obj
    ((or (? public-option?) (? private-option?) (? open-option?)) #t)
    (_ #f)))


;;;; Configuration

(define-immutable-record-type <configuration>
  (mecha-configuration name dir values terse long)
  configuration?
  (name   configuration-name   set-configuration-name)
  (dir    configuration-dir    set-configuration-dir)
  (values configuration-values set-configuration-values)
  (terse  configuration-terse  set-configuration-terse)
  (long   configuration-long   set-configuration-long))

(define* (define-configuration name terse values #:key config-dir
           long help? usage? version? (version-test? string?))
  "Return a configuration.  NAME should be a symbol naming the configuration.
TERSE is a < 40 char description; VALUES is a list of config-options.  The
optional arguments:
 - CONFIG-DIR: the directory in which a configuration file should be
generated.
 - LONG: a longer documentation string (mainly used in config files).
 - HELP?: if #t, add a public help option to VALUES.
 - USAGE?: if #t, add a public usage option to VALUES.
 - VERSION?: if a value, add a private version-number option to VALUES,
populated with the value for this option.  We will also create a public
version option in VALUES.
 - VERSION-TEST?: a procedure used to validate the version number value.
If omitted, this will default to `string?'."
  (define* (augment-if proc do? values)
    (match do?
      (#t (cons (proc) values))
      (#f values)
      (_ (match (proc do? version-test?)
           ((vrsion vrsion-num) (cons* vrsion vrsion-num values))))))

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
      (augment-if complex-version version?
                  (augment-if usage usage?
                              (augment-if help help? values))))
     ;; Else error value
     (_ (throw 'config-spec
               "VALUES should be a list of options and/or configurations.")))
   (match terse
     ;; FIXME: Also test whether shorter than max-length!
     ((? string?) terse)
     (_ (throw 'config-spec "TERSE should be a string.")))
   (match long
     ((or (? string?) #f) long)
     (_ (throw 'config-spec "LONG should be a string, or #f.")))))

(define (configuration-print configuration)
  (match configuration
    (($ <configuration> name dir values terse long)
     (format #t "~a~%~a~%" name terse)
     (when long (format #t "~%~a~%" long))
     (when dir (format #t "~%Configuration Directory: ~a~%" dir))
     (format #t "~%Values: ~%")
     (match values
       (((or ($ <prioption> name value)
             ($ <openoption> name value)
             ($ <puboption> name value)) ...)
        (for-each (lambda (name value)
                    (format #t "  ~a: ~a~%" name value))
                  name value))))))


;;;; Common Option Convenience

;;;;; Version

(define* (complex-version vrsion test)
  "A special option convenience, returning a 2 element list consisting of a
version option, and a version-number option created with VRSION and
TEST."
  (list (version) (version-number vrsion #:test test)))

(define* (version #:optional (terse "Emit version information, then exit."))
  "An option definition providing a configuration-spec version for the
program. Including this in a config spec will also generate GNU
compliant --version output."
  (define-public-option 'version
    terse
    #:single-char #\V
    #:value #f
    #:test boolean?))

(define* (version-number version #:key (test string?)
                         (terse "The version of our application."))
  "A procedure to define a hidden option containing the version of our
application."
  (define-private-option 'version-number
    terse
    #:value version
    #:test test))

;;;;; Help

(define (help)
  "An option definition ensuring we have GNU coding standard compliant
help output."
  (define-public-option 'help
    "Display a help message, then exit."
    #:single-char #\h
    #:value #f
    #:test boolean?))

;;;;; Usage

(define (usage)
  "An option definition ensuring we have GNU coding standard compliant
help output."
  (define-public-option 'usage
    "Display a help message, then exit."
    #:single-char #\u
    #:value #f
    #:test boolean?))


;;;; Validation

(define (check-name name)
  (match name
    ((? symbol?) name)
    (_ (throw 'config-spec "NAME should be a symbol"))))

(define (check-value value)
  (match value
    (_ value)))

(define (check-test test)
  (match test
    ((? procedure?) test)
    (_ (throw 'config-spec
              "TEST should be a predicate procedure."))))

(define (check-single-char char)
  (match char
    ((or (? char?) #f) char)
    (_ (throw 'config-spec
              "SINGLE-CHAR should be a character or #f."))))

(define (check-terse terse)
  (match terse
    ((and (? string?)
          (? (compose (cut <= <> 40) string-length))) terse)
    (_ (throw 'config-spec
              "TERSE should be a string and less than 40 chars."))))

(define (check-long long)
  (match long
    ((or (? string?) #f) long)
    (_ (throw 'config-spec
              "LONG should be a string."))))

(define (check-cli cli)
  (match cli
    ((? boolean?) cli)
    (_ (throw 'config-spec
              "CLI should be a boolean."))))

;;; config-spec.scm ends here.
