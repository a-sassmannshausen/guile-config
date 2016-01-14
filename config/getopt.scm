;;; Config Getpot --- Configuration Getopt-Long intersection
;;; Copyright © 2015 Alex Sassmannshausen <alex@pompo.co>
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

(define-module (config getopt)
  #:use-module (config spec)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export     (<getopt>
                getopt-print
                getopt-free-params
                getopt-configuration
                derive/merge-config-getopt
                establish-subcommands
                configuration->getopt-spec))

;;;;; GetOpt Record
;;;
;;; <getopt> is a <configuration> wrapped together with getopt's special '()
;;; free params.

(define-record-type <getopt>
  (getopt config free-params)
  getopt?
  (config getopt-configuration)
  (free-params getopt-free-params))

(define* (getopt-print getopt #:optional (port #t))
  "Print the <getopt> GETOPT to stdout or to PORT."
  (configuration-print (getopt-configuration getopt))
  (format #t "Free Parameters: ~a~%" (getopt-free-params getopt)))

;;;; Configuration/Getopt Merging
(define (config->getopt-long config cli-params)
  (getopt-long cli-params (configuration->getopt-spec config)))

;;;;; This procedure is only for the purpose of subcommand derivation.
;;; It will return a wrong result in the following specific case:
;;; - Configuration
;;; - Contains nested sub-configurations
;;; - 2 options anywhere within the configuration, we have the same option
;;;   named identically, one of which is a boolean flag, the other of which is
;;;   a value taking param.
;;; - The cli-params contain a value after the named flag above.
;;; If the boolean was defined before the value taking, the value after the
;;; flag will be considered a free param, even if it was intended as a
;;; value taking.
;;; If the value taking was defined before the boolean, the value after the
;;; flag will be considered the value for the boolean, even if it was
;;; intended as a boolean.
;;;
;;; The former will be a problem for subcommand derivation if the subcommand
;;; is specified after the value taking command line option: our subcommand
;;; parsing algorithm will fail to spot the actual subcommand invoked as it is
;;; hidden by the false first free param.
;;;
;;; The latter will be a problem for subcommand derivation if the subcommand
;;; is specified precisely after the boolean command line option: the
;;; subcommand name will be taken to be the value for the false value taking
;;; param.
;;;
;;; Both these cases are illustrated by tests in the tests file, (the tests
;;; are examples, and thus pass).
(define (derive-free-params config cli-params)
  "Return the free parameters, i.e. non key-word arguments, contained in
CLI-PARAMS with respect to CONFIG."
  ;; We must return the free-params of config.  We use getopt-long to do
  ;; this.  However: config->getopt-long expects a flat config, and we have a
  ;; nested config.  CLI-PARAMS could refer to options from any of the nested
  ;; configs.  Hence we must "collapse" all nested options into one giant list
  ;; of options, with all "validation cleared", so that the
  ;; config->getopt-long conversion is guaranteed to succeed.
  (define (clear-validation opt)
    "Return a new version of OPT that is stripped of all validation."
    (match opt
      ((name . ($ <openoption> name value test _ single-char))
       (cons name (open-option name "" #:single-char single-char
                    #:test (if (eq? (procedure-name test) 'boolean?)
                               test
                               (const #t))
                    #:optional? #t #:value value)))
      ((name . ($ <puboption> name value test _ single-char))
       (cons name (public-option name "" #:single-char single-char
                    #:test (if (eq? (procedure-name test) 'boolean?)
                               test
                               (const #t))
                    #:optional? #t #:value value)))
      ((name . (? private-option?)) opt)))
  (define (collapse configs options)
    "Return either OPTIONS, or the product of appending to OPTIONS the result
of further collapsing CONFIGS, if CONFIGS is not null."
    (match configs
      (() options)
      (configs
       (append options
               (fold (lambda (current flattened)
                       (match current
                         ((name . ($ <configuration> _ _ options configs))
                          (append  flattened (collapse configs options)))))
                     '()
                     configs)))))

  (option-ref (config->getopt-long
               (match config
                 (($ <configuration> _ _ _ ()) config)
                 (($ <configuration> _ _ opts confs)
                  (set-configuration-options config
                                             (map clear-validation
                                                  (collapse confs opts)))))
               cli-params)
              '() '()))

(define (expand-configs configs)
  "Return configs, augmented by 'pseudo-configs' which are built out of the
`config-alias' fields of each config, if it is defined."
  (fold (lambda (current expanded-configs)
          (match current
            ((name . config)
             (match (configuration-alias config)
               (#f (cons current expanded-configs))
               (alias (cons* (cons (configuration-alias config) config)
                        current
                        expanded-configs))))))
        '()
        configs))

(define (establish-subcommands configuration cli-params)
  "Return a breadcrumb trail leading to the requested subcommand that is part
of CONFIGURATION and requested by CLI-PARAMS."
  ;; FIXME: currently, a free-param to a program that accidentally matches the
  ;; first subcommand name, will trigger that subcommand rather than acting as
  ;; that command's free-param.
  ;; Alternatively, correctly implementing the special '--' param would allow
  ;; us to fully deal with the above problem, and would allow us to retain the
  ;; current flexibility (mixed cli-params, subcommands) [and the current
  ;; cost].
  (let establish ((free-params (derive-free-params configuration cli-params))
                  (subcommands '())
                  ;; We expand-configs to handle the use of aliases on the
                  ;; commandline.  Expanding ensures an alias will match to
                  ;; its appropriate configuration.
                  (configs     (expand-configs
                                (configuration-configs configuration))))
    (match free-params
      (((? string? candidate) . rest)
       (match (assq (string->symbol candidate) configs)
         ((k . ($ <configuration> name _ opts configs _ _))
          (establish rest (cons name subcommands)
                     (expand-configs configs)))
         ;; Could be #f or an <option>
         (_ (reverse subcommands))))
      ;; We have no free params (i.e. no subcommand specified).
      (() (reverse subcommands)))))

(define (derive/merge-config-getopt config cli-params)
  "Return the <getopt> resulting from merging the list CLI-PARAMS into
<configuration> CONFIG."
  (apply getopt (merge-config-getopt config
                                     (config->getopt-long config
                                                          cli-params))))

(define (merge-config-getopt config getopts)
  "Return the <configuration> resulting from merging the getopt-long interface
GETOPTS (normally the list of commandline arguments to a program) into
<configuration> CONFIG."
  (list (set-configuration-options config
                              (map (getopt-merger getopts)
                                   (configuration-options config)))
        (option-ref getopts '() '())))

(define (getopt-merger getopts)
  "Return a procedure taking a configuration-value from a <configuration>,
which returns an updated configuration-value if an updated values is present
in the getopt-long interface GETOPTS.  Otherwise that procedure will simply
return the original configuration-value."
  (define (opt-if setter cli-value option-name handler)
    "Return either a new configuration-value built with SETTER, OPTION-NAME
and CLI-VALUE, or just the original configuration-value OPTION-NAME."
    (if cli-value (setter option-name (handler cli-value)) option-name))
  (lambda (config-val)
    (match config-val
      ((name . (? public-option? opt))
       (cons name (opt-if set-puboption-value (option-ref getopts name #f) opt
                          (puboption-handler opt))))
      ((name . (? open-option? opt))
       (cons name (opt-if set-openoption-value (option-ref getopts name #f)
                          opt (openoption-cli-handler opt))))
      (_ config-val))))


;;;; Configuration Spec parsing

;;;;; Option Parsing

(define (configuration->getopt-spec config)
  "Return the getopt-long option-spec corresponding to CONFIG.  Any private
options will be ignored, as they have no getopt-long config (they have no
commandline capability)."
  (reverse (fold (lambda (opt converted)
                   (match opt
                     ((name . (? public-option? opt))
                      (cons (puboption->getopt-spec opt) converted))
                     ((name . (? open-option? opt))
                      (cons (openoption->getopt-spec opt) converted))
                     (_ converted)))
                 '()
                 (configuration-options config))))

(define* (getopt-spec name test handler single-char optional?
                      #:optional required)
  "Create a getopt-long spec entry from NAME, TEST, SINGLE-CHAR and
REQUIRED."
  (define (value-entry)
    (match (procedure-name test)
      ;; If our test is boolean, we parse params as flags
      ('boolean? '((value #f)))
      ;; If optional?, parse param value as flag or value.
      (_  (if optional?
              '((value optional))
              '((value #t))))))

  (apply list name `(predicate ,(compose test handler)) `(required? ,required)
         (match single-char
           ((? char?) (cons `(single-char ,single-char)
                           (value-entry)))
           (#f        (value-entry)))))

(define (openoption->getopt-spec openoption)
  "Return the getopt-long option-spec for OPENOPTION."
  (match openoption
    (($ <openoption> name '<unset> test cli-handler single-char _ _ _
                     optional)
     (getopt-spec name test cli-handler single-char optional #t))
    (($ <openoption> name _ test cli-handler single-char _ _ _ optional)
     (getopt-spec name test cli-handler single-char optional))))

(define (puboption->getopt-spec puboption)
  "Return the getopt-long option-spec for PUBOPTION."
  (match puboption
    (($ <puboption> name '<unset> test handler single-char _ _ _ optional)
     (getopt-spec name test handler single-char optional #t))
    (($ <puboption> name _ test handler single-char _ _ _ optional)
     (getopt-spec name test handler single-char optional))))

;;; getopt.scm ends here.
