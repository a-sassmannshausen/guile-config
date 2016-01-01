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

(define-module (config spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (
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
            puboption-handler
            set-puboption-handler
            puboption-single-char
            set-puboption-single-char
            puboption-terse
            set-puboption-terse
            puboption-long
            set-puboption-long
            puboption-optional
            set-puboption-optional
            define-public-option

            <openoption>
            open-option?
            openoption-name
            set-openoption-name
            openoption-value
            set-openoption-value
            openoption-test
            set-openoption-test
            openoption-cli-handler
            set-openoption-cli-handler
            openoption-single-char
            set-openoption-single-char
            openoption-terse
            set-openoption-terse
            openoption-long
            set-openoption-long
            openoption-optional
            set-openoption-optional
            define-open-option

            option?

            <configuration>
            mecha-configuration
            configuration?
            configuration-name
            set-configuration-name
            configuration-dir
            set-configuration-dir
            configuration-options
            set-configuration-options
            configuration-configs
            set-configuration-configs
            configuration-terse
            set-configuration-terse
            configuration-long
            set-configuration-long
            configuration-parser
            set-configuration-parser
            configuration-alias
            set-configuration-alias

            configuration-file
            configuration-write
            configuration-read

            complex-version
            version
            version-number
            help
            usage
            author-maker
            check-name
            copyright-maker
            license
            license-maker
            license->string
            license?
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
  (mecha-puboption name value test handler single-char terse long example
                   optional)
  public-option?
  (name puboption-name set-puboption-name)
  (value puboption-value set-puboption-value)
  (test puboption-test set-puboption-test)
  (handler puboption-handler set-puboption-handler)
  (single-char puboption-single-char set-puboption-single-char)
  (terse puboption-terse set-puboption-terse)
  (long puboption-long set-puboption-long)
  (example puboption-example set-puboption-example)
  (optional puboption-optional set-puboption-optional))

(define* (define-public-option name terse #:key long
           (value '<unset>) single-char (test boolean?) (handler identity)
           (example "VALUE") optional?)
  "Return a Public Option.  NAME should be a symbol naming the option and
TERSE should be a < 40 char decsription.
 - LONG: space for a longer description.
 - VALUE: the value assigned to this private option.
 - SINGLE-CHAR: if set to a character, the single-char for the getopt-long
spec assigned to this option.
 - TEST: the predicate to check this VALUE against.
 - HANDLER: a procedure of one argument to apply to the string as provided
from the commandline.  This should be a transformer from
string->value-for-test.
 - EXAMPLE: an example cli value for help purposes.
 - OPTIONAL?: a boolean to inform us that the cli argument can, but doesn't
have to take an argument."
  (mecha-puboption (check-name name)
                   (check-value value)
                   (check-test test)
                   (check-handler handler)
                   (check-single-char single-char)
                   (check-terse terse)
                   (check-long long)
                   (check-example example)
                   (check-optional optional?)))

;;;;; Open Options
;;;
;;; Open options are options that have default values, which can be
;;; overridden in configuration files, and which can be overriden at
;;; the CLI.
(define-immutable-record-type <openoption>
  (mecha-openoption name value test cli-handler single-char terse long example
                    optional)
  open-option?
  (name openoption-name set-openoption-name)
  (value openoption-value set-openoption-value)
  (test openoption-test set-openoption-test)
  (cli-handler openoption-cli-handler set-openoption-cli-handler)
  (single-char openoption-single-char set-openoption-single-char)
  (terse openoption-terse set-openoption-terse)
  (long openoption-long set-openoption-long)
  (example openoption-example set-openoption-example)
  (optional openoption-optional set-openoption-optional))

(define* (define-open-option name terse #:key single-char
           (value '<unset>) (test boolean?) (cli-handler identity) long
           (example "VALUE") optional?)
  "Return a Public Option.  NAME should be a symbol naming the option and
TERSE should be a < 40 char decsription.
 - LONG: space for a longer description.
 - VALUE: the value assigned to this private option.
 - SINGLE-CHAR: if set to a character, the single-char for the getopt-long
spec assigned to this option.
 - TEST: the predicate to check this VALUE against, after applying the
relevant handler.
 - CLI-HANDLER: a procedure of one argument to apply to the string as provided
from the commandline.  This should be a transformer from
string->value-for-test.
 - EXAMPLE: an example cli value for help purposes.
 - OPTIONAL?: a boolean to inform us that the cli argument can, but doesn't
have to take an argument."
  (mecha-openoption (check-name name)
                    (check-value value)
                    (check-test test)
                    (check-handler cli-handler)
                    (check-single-char single-char)
                    (check-terse terse)
                    (check-long long)
                    (check-example example)
                    (check-optional optional?)))

;;;;; Generic Option Predicate

(define (option? obj)
  "Return #t if OBJ is an option; #f otherwise."
  (match obj
    ((or (? public-option?) (? private-option?) (? open-option?)) #t)
    (_ #f)))


;;;; Configuration

(define-immutable-record-type <configuration>
  (mecha-configuration name dir options configs terse long parser alias)
  configuration?
  (name    configuration-name    set-configuration-name)
  (dir     configuration-dir     set-configuration-dir)
  (options configuration-options set-configuration-options)
  (configs configuration-configs set-configuration-configs)
  (terse   configuration-terse   set-configuration-terse)
  (long    configuration-long    set-configuration-long)
  (parser  configuration-parser  set-configuration-parser)
  (alias   configuration-alias   set-configuration-alias))

(define (configuration-print configuration)
  (match configuration
    (($ <configuration> name dir opts configs terse long)
     (format #t "~a~%~a~%" name terse)
     (when long (format #t "~%~a~%" long))
     (when dir (format #t "~%Configuration Directory: ~a~%" dir))
     (format #t "~%Values: ~%")
     (match opts
       (((or (name . ($ <prioption> name value))
             (name . ($ <openoption> name value))
             (name . ($ <puboption> name value))) ...)
        (for-each (lambda (name value)
                    (format #t "  ~a: ~a~%" name value))
                  name value)))
     (format #t "~%Configurations: ~%")
     (match configs
       (((name . (? configuration? configuration)) ...)
        (for-each configuration-print configuration))))))

(define (configuration-file configuration)
  "Return the full filename of the CONFIGURATION file we want to use, or #f if
we don't have a configuration dir."
  (if (configuration-dir configuration)
      (string-append (configuration-dir configuration)
                     file-name-separator-string
                     (symbol->string (configuration-name configuration)))
      #f))


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

;;;;; License

(define-record-type <license>
  (license id name url)
  license?
  (id license-id)
  (name license-name)
  (url license-url))

(define (license-maker obj values)
  "Analyse OBJ and return a <license> constructed from it."
  (match obj
    ('gplv3+ (cons (license-gplv3+) values))
    ('agplv3+ (cons (license-agplv3+) values))
    ((? string?) (cons (license-generic obj) values))
    ((? license?) (cons (define-private-option 'license
                          "The license of this project."
                          #:value obj
                          #:test license?)
                        values))
    (_ (throw 'license-maker
              "Invalid LICENSE: should be 'gplv3+, 'agplv3+, a license object
or a string naming a license."))))

(define (license->string license)
  "Turn <license> LICENSE into a string for summary and/or printing."
  (match license
    (($ <license> #f name #f)
     (string-join `("License:" ,name) " "))
    (($ <license> id name url)
     (string-append "License " id ": " name " <" url ">"))
    (_ (throw 'license->string "Unexpected LICENSE."))))

(define (license-generic name)
  "Generate a simple <license> out of NAME."
  (define-private-option 'license
    "The license of this project."
    #:value (license #f name #f)
    #:test license?))

(define (license-gplv3+)
  "Return a <license> representing the GPLv3+."
  (define-private-option 'license
    "The license of this project."
    #:value (license "GPLv3+" "GNU GPL version 3 or later"
                     "http://gnu.org/licenses/gpl.html")
    #:test license?))

(define (license-agplv3+)
  "Return a <license> representing the AGPLv3+."
  (define-private-option 'license
    "The license of this project."
    #:value (license "AGPLv3+" "GNU AGPL version 3 or later"
                     "http://gnu.org/licenses/agpl.html")
    #:test license?))

;;;;; Copyright

(define (copyright-maker years)
  "Return a private option representing the YEARS of copyright for a project."
  (define-private-option 'copyright
    "The years for which we claim copyright."
    #:value years
    #:test (lambda (x) (match x (((? integer? years) ...) #t) (_ #f)))))

;;;;; Author

(define (author-maker author)
  "Return a private option detailing the AUTHOR of this project."
  (define-private-option 'author
    "The author of this project."
    #:value author
    #:test string?))

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

(define (check-handler handler)
  (match handler
    ((? procedure?) handler)
    (_ (throw 'config-spec
              "HANDLER should be a procedure."))))

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
    (_ (throw 'config-spec "LONG should be a string."))))

(define (check-cli cli)
  (match cli
    ((? boolean?) cli)
    (_ (throw 'config-spec "CLI should be a boolean."))))

(define (check-example example)
  (match example
    ((? string?) example)
    (_ (throw 'config-spec "EXAMPLE should be a string."))))

(define (check-optional optional)
  (match optional
    ((? boolean?) optional)
    (_ (throw 'config-spec "OPTIONAL should be a boolean."))))

;;; config-spec.scm ends here.
