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

(define-module (config getopt-long)
  #:use-module (config api)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (read-commandline))

(define (read-commandline commandline codex)
  "Return a codex, with commandline merged into codex, getopt-long style."
  ;; Turn Codex into getopt-long construct, then query getopt-long with
  ;; commandline values and feed those back into codex.
  ;;
  ;; Specifically we pass reagents modified (not raw) commandline values to
  ;; getopt-long.

  ;; Replace codex with updated codex
  ;; getopt-long here causes exits.  For testing purposes these can be caught
  ;; with catch of 'quit signal.
  ;; The error messages it emits are:
  ;; $script: option must be specified: --$option
  ;; $script: no such option: --$option
  ;; $script: option must be specified with argument: --$option
  (define (augment-keywords)
    (filter-map (match-lambda
                  ((id chr #t)
                   (switch
                    (name id) (character chr) (test boolean?) (default #f)
                    (handler identity) (optional? #f))) 
                  ((_ _ (or #f ($ <empty>))) #f))
                `((help #\h ,(codex-metadatum 'generate-help? codex))
                  (usage #f ,(codex-metadatum 'generate-usage? codex))
                  (version #f ,(codex-metadatum 'generate-version? codex)))))

  (let* ((vls (codex-valus codex))
         (kwds (append (valus-keywords vls) (augment-keywords)))
         ;; FIXME: dummy should be subcommand-path
         (gtl (getopt-long (cons "dummy" commandline)
                           (codex->getopt-spec kwds))))
    (set-codex-valus codex 
                     (valus (map (lambda (kwd)
                                   (set-keyword-default
                                    kwd
                                    (option-ref gtl (keyword-name kwd)
                                                (keyword-default kwd))))
                                 kwds)
                            (valus-arguments vls)))))

(define (codex->getopt-spec keywords)
  "Return the getopt-long option-spec corresponding to the <setting> and
    <switch> keywords in KEYWORDS."
  (reverse
   (fold
    (lambda (kwd done)
      (match kwd
        (($ <switch> name ($ <empty>) test handler character _ _ _ optional)
         (cons (getopt-spec name test handler character optional #t) done))
        (($ <switch> name _ test handler character _ _ _ optional)
         (cons (getopt-spec name test handler character optional #f) done))
        (($ <setting> name ($ <empty>) test handler character _ _ _ optional)
         (cons (getopt-spec name test handler character optional #t) done))
        (($ <setting> name _ test handler character _ _ _ optional)
         (cons (getopt-spec name test handler character optional #f) done))
        (_ done)))
    '()
    keywords)))

(define (getopt-spec name test handler single-char optional? required)
  "Create a getopt-long spec entry from NAME, TEST, HANDLER, SINGLE-CHAR,
    OPTIONAL? and REQUIRED."
  (define (value-entry)
    (match (procedure-name test)
      ;; If our test is boolean, we parse params as flags
      ('boolean? '((value #f)))
      ;; If optional?, parse param value as optional, else as valued.
      (_  (if optional?
              '((value optional))
              '((value #t))))))

  (apply list name `(predicate ,(compose test handler)) `(required? ,required)
         (match single-char
           ((? char?) (cons `(single-char ,single-char)
                            (value-entry)))
           (#f        (value-entry)))))
