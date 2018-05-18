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

(define (read-commandline commandline settings codex)
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
    "Return additional switches for help, usage, version, if requested."
    (filter-map (match-lambda
                  ((id chr #t)
                   (switch
                    (name id) (character chr) (test boolean?) (default #f)
                    (handler identity) (optional? #f))) 
                  ((_ _ (or #f ($ <empty>))) #f)
                  (n (throw 'augment-keywords "no matching pattern" n)))
                `((help #\h ,(codex-metadatum 'generate-help? codex))
                  (usage #f ,(codex-metadatum 'generate-usage? codex))
                  (version #f ,(codex-metadatum 'generate-version? codex))
                  (cmdtree #f ,(codex-metadatum 'generate-cmdtree? codex)))))

  (let* ((vls (codex-valus codex))
         (kwds (append (valus-keywords vls) (augment-keywords)))
         (args (valus-arguments vls))
         (gtl (getopt-long
               ;; Here we insert the subcommand path to the command we're
               ;; executing in commandline, so that getopt-long emits the full
               ;; path
               (cons (string-join (cons "error:" (full-command codex)))
                     commandline)
               (codex->getopt-spec kwds))))
    (set-codex-valus
     codex
     (valus
      (map (lambda (kwd)
             (if (secret? kwd)
                 kwd                    ; <secret> is never updated
                 (let ((kwd-name (keyword-name kwd)))
                   (match (option-ref gtl kwd-name (empty))
                     (($ <empty>)
                      (match (assoc kwd-name settings)
                        ((name . value)
                         (set-keyword-default
                          kwd (test-kwd/arg name ((keyword-handler kwd) value)
                                            (keyword-test kwd) codex
                                            "keyword")))
                        (#f kwd)))
                     (value
                      (set-keyword-default kwd ((keyword-handler kwd)
                                                value)))))))
           kwds)
      ;; Arguments can't be retrieved by name with getopt-long.  Instead,
      ;; fetch all args, then handle them ourselves.
      (parse-arguments args (option-ref gtl '() '()) codex)))))

(define* (parse-arguments arguments cmd-values codex #:optional (result '()))
  "Cycle through ARGUMENTS, the configuration's defined arguments, &
CMD-VALUES the arguments provided on the command-line, returning a new list of
arguments in RESULT after testing them & updating them from CMD-VALUES."
  (cond ((null? arguments)
         ;; Processed all arguments, -> done.
         (reverse result))
        ((null? cmd-values)
         ;; We're out of cmdline arguments, -> defaults for rest
         (append (reverse result) (map (cut optional-argument <> codex)
                                       arguments)))
        (else
         (parse-arguments
          (cdr arguments)
          (cdr cmd-values)
          codex
          (cons
           (set-argument-default
            (first arguments)
            (test-kwd/arg
             ((argument-handler (first arguments)) (first cmd-values))
             (argument-name (first arguments))
             (argument-test (first arguments))
             codex "argument"))
           result)))))

(define (optional-argument argument codex)
  "Return ARGUMENT if it is optional or emit an error."
  (match (argument-optional? argument)
    (#t argument)
    (_
     (format
      ;; We want to emit:
      ;; error: FULL-COMMAND: argument must be specified: NAME
      #t "error: ~a: argument must be specified: ~a~%"
      (string-join (full-command codex)) (argument-name argument))
     (throw 'quit 'optional-arg))))

(define (test-kwd/arg value name test codex type)
  "Return VALUE if it passes TEST or throw an error pointing at NAME of TYPE."
  (match (test value)
    (#f
     (format
      ;; We want to emit:
      ;; error: FULL-COMMAND: ARGUMENT|KEYWORD predicate failed: [--]NAME
      #t "error: ~a: ~a predicate failed: ~a~a~%"
      (string-join (full-command codex)) type
      (match type ("argument" "") (_ "--")) name)
     (throw 'quit 'test-kwd/arg))
    (value value)))

(define (codex->getopt-spec keywords)
  "Return the getopt-long option-spec corresponding to the <setting> and
    <switch> keywords in KEYWORDS."
  (reverse
   (fold
    (lambda (kwd done)
      (let ((character (keyword-character kwd)))
        (match kwd
          (($ <switch> name ($ <empty>) test handler _ _ _ _ #f)
           ;; A switch is only mandatory if optional is #f
           (cons (getopt-spec name test handler character #f #t) done))
          (($ <switch> name _ test handler _ _ _ _ optional)
           (cons (getopt-spec name test handler character optional #f) done))
          (($ <setting> name ($ <empty>) test handler _ _ _ _ optional)
           (cons (getopt-spec name test handler character optional #t) done))
          (($ <setting> name _ test handler _ _ _ _ optional)
           (cons (getopt-spec name test handler character optional #f) done))
          (_ done))))
    '()
    (filter (negate secret?)
            keywords))))

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
           (#f        (value-entry))
           (n (throw 'getopt-spec "no matching pattern" n)))))
