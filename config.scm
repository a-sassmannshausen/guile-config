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
  #:use-module (config spec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (monads)
  #:use-module (monads io)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:re-export  (define-configuration define-private-option
                 define-public-option define-open-option)
  #:export     (getopt-config
                getmio-config
                option-ref
                make-help-emitter))


;;;; Porcelain

(define (getopt-config args config)
  (run-io (process-configuration config args)))

(define (getmio-config args config)
  (process-configuration config args))

(define (option-ref configuration key default)
  ;; Not implemented the special key '() which should return all non-option
  ;; arguments (see (ice-9 getopt-long option-ref) for details).
  (match configuration
    (($ <configuration> _ _ values _ _)
     (match (assq key values)
       (#f default)
       ((k . v) (match v
                  (($ <puboption> n v) v)
                  (($ <prioption> n v) v)
                  (($ <openoption> n v) v)))))))

(define (make-help-emitter config)
  "Traverse config, building a GNU-style help message as we do so.  Return a
procedure that would emit such a message."
  (match (configuration-values config)
    (((names . (? option? opts)) ...)
     (lambda* (#:optional (port (current-output-port)))
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
                   (fill-paragraph (configuration-long config) 80)))))

    (((name . (or ($ <configuration>) (? option? opts))) ...)
     (throw 'config "CONFIGURATION in help is not yet supported."))
    (_ (throw 'config "CONFIG is invalid."))))


;;;; Plumbing

(define (process-configuration configuration cli-params)
  (mlet* %io-monad
      ;; Remember, we may have nested configurations!
      (;; Hence recurse at every monadic proc through the whole of config.
       (null          ((lift ensure-config-files %io-monad) configuration))
       (merged-config ((lift merge-config-file-values %io-monad)
                       configuration)))
    (return (merge-config-cli-values merged-config cli-params))))

;; merge-config-cli-values is simple io based: it is functional, in an
;; io-monad wrapper.
(define (merge-config-cli-values configuration cli-params)
  (derive/merge-config-getopt configuration cli-params))

;; The values are read from configuration files, specified in configuration,
;; and modifying configuration.
;; => io read -> configuration.
(define (merge-config-file-values configuration)
  configuration)

;; We check the configuration file paths from CONFIGURATION, and as output
;; side-effects, create those files if they do not yet exist, populated with
;; values from CONFIGURATION.
(define (ensure-config-files configuration)
  configuration)

;; Then we should export the high-level bindings to:
;;   + extract documentation for configuration
;;   + extract full configuration specification in readible way


;;;; Helpers

(define* (fill-paragraph str width #:optional (column 0))
  "Fill STR such that each line contains at most WIDTH characters, assuming
that the first character is at COLUMN.

When STR contains a single line break surrounded by other characters, it is
converted to a space; sequences of more than one line break are preserved."
  (define (maybe-break chr result)
    (match result
      ((column newlines chars)
       (case chr
         ((#\newline)
          `(,column ,(+ 1 newlines) ,chars))
         (else
          (let* ((spaces (if (and (pair? chars) (eqv? (car chars) #\.)) 2 1))
                 (chars  (case newlines
                           ((0) chars)
                           ((1)
                            (append (make-list spaces #\space) chars))
                           (else
                            (append (make-list newlines #\newline) chars))))
                 (column (case newlines
                           ((0) column)
                           ((1) (+ spaces column))
                           (else 0))))
            (let ((chars  (cons chr chars))
                  (column (+ 1 column)))
              (if (> column width)
                  (let*-values (((before after)
                                 (break (cut eqv? #\space <>) chars))
                                ((len)
                                 (length before)))
                    (if (<= len width)
                        `(,len
                          0
                          ,(if (null? after)
                               before
                               (append before
                                       (cons #\newline
                                             (drop-while (cut eqv? #\space <>)
                                                         after)))))
                        `(,column 0 ,chars)))     ; unbreakable
                  `(,column 0 ,chars)))))))))

  (match (string-fold maybe-break
                      `(,column 0 ())
                      str)
    ((_ _ chars)
     (list->string (reverse chars)))))

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

;;; config ends here
