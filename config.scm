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

(define-module (config)
  #:use-module (config api)
  #:use-module (config getopt-long)
  #:use-module (config helpers)
  #:use-module (config licenses)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (getopt-config-auto
            getopt-config
            option-ref
            options-write

            emit-help emit-version))

;;;; UI

(define* (option-ref codex key #:optional default)
  "Return the value for KEY in CODEX, or DEFAULT if it cannot be found. KEY
should be either:
- a symbol, to retrieve the respective keyword in CODEX,
- a list of exactly one symbol, to retrieve the respective argument in CODEX.
- '(), to retrieve all arguments, without additional processing."
  (let ((valus (codex-valus codex)))
    (match key
      (() (valus-arguments valus))
      ((key) (or (find-argument key (valus-arguments valus)) default))
      (key (or (find-keyword key (valus-keywords valus)) default))
      (n (throw 'option-ref "no matching pattern" n)))))

(define* (getopt-config-auto commandline configuration)
  "Return a <codex>, generated from CONFIGURATION applied to COMMANDLINE only
if no help, usage, version or cmdtree was requested."
  (let ((cdx (getopt-config commandline configuration)))
    (cond ((or (option-ref cdx 'help) (option-ref cdx 'usage))
           (emit-help cdx)
           (exit 0))
          ((option-ref cdx 'version)
           (emit-version cdx)
           (exit 0))
          ((option-ref cdx 'cmdtree)
           (emit-cmdtree cdx)
           (exit 0))
          (else cdx))))

(define (getopt-config commandline configuration)
  "Return a <codex>, generated from CONFIGURATION applied to COMMANDLINE.

Values from this codex can be extracted using `OPTION-REF'."
  (let* ((reagents (subcommand-reagents commandline configuration))
         ;; Configuration fields are never overriden by commandline or
         ;; configuration file values, so we can resolve inheritance of those
         ;; fields before dealing with files & commandline.  So we generate a
         ;; codex here, to be augmented by values parsed from the
         ;; configuration files & commandline switches.
         (cdx (codex
               (apply features (map (lambda (proc)
                                      (proc (inverted-next-config
                                             (reagents-inverted reagents))))
                                    (list configuration-name
                                          configuration-synopsis
                                          configuration-description
                                          configuration-alias
                                          configuration-subcommands)))
               (apply metadata (metadata-fetch (reagents-inverted reagents)))
               (apply valus (valus-fetch (reagents-inverted reagents)))
               reagents)))
    ;; We should try to ensure eager configuration files exist now: write them
    ;; if non-existing
    (options-write cdx configuration #t)
    ;; Try parsing commandline and config file.  On error...
    (catch 'quit
      (lambda _
        (read-commandline (reagents-commandline reagents)
                          (options-read cdx configuration) cdx))
      ;; Emit help if requested, else quit
      (lambda (k vals)
        (when (configuration-generate-help? configuration)
          (emit-help cdx))
        (exit 1)))))

(define* (options-write codex #:optional configuration eager-only?)
  "Write configuration files in CODEX.  If EAGER-ONLY? is #t, write only the
eager configuration files. If CONFIGURATION is provided, instead, try to write
the complete configuration configuration files."
  (let ((parser (codex-metadatum 'parser codex))
        (config-file (metadata-directory (codex-metadata codex))))
    (when (not (empty? config-file))
      (if (single-configuration-file? config-file)
          (when (or (and eager-only? (path-eager? config-file))
                    (not eager-only?))
            (or (and=> configuration parser-write-complete)
                (write-codex parser config-file (codex-features codex)
                             (codex-valus codex))))
          (for-each (lambda (path)
                      (when (or (and eager-only? (path-eager? path))
                                (not eager-only?))
                        (or (and=> configuration parser-write-complete)
                            (write-codex parser path
                                         (codex-features codex)
                                         (codex-valus codex)))))
                    config-file)))))

(define (options-read codex configuration)
  (let ((parser (codex-metadatum 'parser codex))
        (config-file (metadata-directory (codex-metadata codex)))
        (feature-name (features-name (codex-features codex))))
    (cond ((empty? config-file) '())    ; No config file -> no settings
          ((single-configuration-file? config-file)
           ;; Either read complete or just this codex's config file.
           (or (parser-read-complete configuration config-file)
               (parser-read parser
                            (parser-file parser config-file feature-name))))
          ((n-configuration-files? config-file)
           ;; For each config-file, either read complete or just this codex's
           ;; config file, with later config-files overriding earlier ones.
           (let ((htable (make-hash-table)))
             (for-each
              (lambda (path)
                (for-each
                 (match-lambda
                   ((n . d) (hash-set! htable n d)))
                 (or (parser-read-complete configuration path)
                     (parser-read parser (parser-file parser path
                                                      feature-name)))))
              config-file)
             (hash-map->list cons htable))))))


;;;;; Helpers

;; Rules for metadata inheritance:
;; if field <empty>, check field in next ancestor, ... Select first value or
;; retain <empty> if all <empty>.
(define (metadata-fetch inverted)
  "Return the metadata values with inheritance resolved, for INVERTED."
  (map (lambda (getter)
         (let lp ((current-entry (inverted-next-config inverted))
                  (rest (cdr inverted)))
           (match (getter current-entry)
             ((or (? not value) (? empty? value)
                  (? (cute equal? identity-parser <>) value))
              (if (null? rest)
                  value
                  (lp (inverted-next-config rest)
                      (cdr rest))))
             (value value)
             (n (throw 'metadata-fetch "no matching pattern" n)))))
       (list configuration-directory
             configuration-version
             configuration-license
             configuration-copyright
             configuration-author
             configuration-parser
             configuration-generate-help?
             configuration-generate-usage?
             configuration-generate-version?
             configuration-generate-cmdtree?)))

;; Rules for valus inheritance:
;; The set of valus for child := child-valus [+ ancestor valus wanted]
;; However, as we are traversing parent *configurations*, inheritance has not
;; percolated through them.  We must recurse through ancestors until we have
;; all we wanted.
(define (valus-fetch inverted)
  "Return the valus, with inheritance resolved, for INVERTED."
  (let ((self (inverted-next-config inverted)))
    (define (fetch type-of-wealth self-proc proc)
      (let lp ((ancestors (cdr inverted))
               (wealth (self-proc self))
               (wanted (or (assoc-ref (configuration-wanted self)
                                      type-of-wealth)
                           '())))
        (cond ((null? wanted) wealth)
              ((and (null? ancestors) (not (null? wanted)))
               ;; Reached end of ancestors but still wanting
               (throw 'valus-fetch 'still-wanting wanted))
              ;; Normal case: fetch wanted, then recurse
              (else
               (let* ((parent-wealth (proc (inverted-next-config ancestors)))
                      (new-wealth (fold (lambda (n p)
                                          (match (assoc-ref parent-wealth n)
                                            ;; Not found, still wanted
                                            (#f `(,(first p)
                                                  ,(cons n (second p))))
                                            ;; In parent, add to wealth
                                            (e `(,(cons e (first p))
                                                 ,(second p)))))
                                        '(() ())
                                        wanted)))
                 (lp (cdr ancestors)
                     (append (first new-wealth) wealth)
                     (second new-wealth)))))))
    (list
     (fetch 'keywords configuration-keywords
            (compose (cute map (lambda (n) (cons (keyword-name n) n)) <>)
                     configuration-keywords))
     (fetch 'arguments configuration-arguments
            (compose (cute map (lambda (n) (cons (argument-name n) n)) <>)
                     configuration-arguments)))))


;;;; Emitters

(define* (emit-help codex #:optional (port #t))
  "Traverse the config in CODEX, building a GNU-style help message as we do
so and emit it to PORT."
  (define (filter-keywords keywords)
    (filter (lambda (x) (or (switch? x) (setting? x))) keywords))

  (let* ((valus    (codex-valus codex))
         (keywords (valus-keywords valus))
         (usage-string (string-append "Usage" ": "))
         (name (string-join (full-command codex))))
    ;; Short Help
    (format port "~a~a ~a~%" usage-string name
            (sort-keywords (filter-keywords keywords)
                           (+ (string-length usage-string)
                              (string-length name)
                              1)
                           (valus-arguments valus)))
    ;; Detailed Help
    (format port "~%~a~%~a~%" (string-append "Options" ":")
            (sort-detailed-keywords (filter-keywords keywords)))
    ;; Subcommand listing
    (match (sort-subcommands (codex-feature 'subcommands codex))
      ("" #f)
      (subcommands
       (format port "~%~a~%~a~%" (string-append "Subcommands" ":")
               subcommands))
      (n (throw 'emit-help "no matching pattern" n)))
    ;; Description
    (match (codex-feature 'description codex)
      ((? (negate empty?) desc)
       (format port "~%~a~%" (fill-paragraph desc 80)))
      (n (throw 'emit-help "no matching pattern" n)))))

(define* (emit-version codex #:optional (port #t))
  "Traverse the config in CODEX, building a GNU-style version message as we
do so and emit it to PORT."
  (format port "~a~%~a~a~a~%"
          (string-join
           (append (full-command codex)
                   (match (codex-metadatum 'version codex)
                     (($ <empty>) '())
                     ((? string? version) `(,version))
                     ((? number? version) `(,(number->string version)))
                     (n (throw 'emit-version "no matching pattern" n)))))
          (match (map (cut codex-metadatum <> codex) '(copyright author))
            ((or (($ <empty>) _) (_ ($ <empty>))) "")
            ((years author)
             (string-append "Copyright (C) "
                            (string-join (map number->string years) ", ")
                            " " author "\n"))
            (n (throw 'emit-version "no matching pattern" n)))
          (match (codex-metadatum 'license codex)
            ((? license? license)
             (string-append (license->string license) "\n"))
            (_ ""))
          "This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."))

(define* (emit-cmdtree codex #:optional (port #t))
  "Emit the command tree associated with the configuration stored in CODEX to
PORT, defaulting to stdout."
  (define (next-string config prefix)
    (string-append prefix "- " (symbol->string (configuration-name config))
                   (match (configuration-synopsis config)
                     ("" "")
                     (syn (string-append ": " syn)))))

  (format port "Command tree overview of ~a~%~%~a~%"
          (codex-feature 'name codex)
          (let ((config (inverted-next-config
                         (reagents-inverted (codex-reagents codex)))))
            (let lp ((subcmd config)
                     (result (next-string config ""))
                     (prefix "  "))
              (match (configuration-subcommands subcmd)
                (() result)
                (subcmds
                 (string-join
                  (cons result
                        (map (lambda (subcmd)
                               (lp subcmd
                                   (next-string subcmd prefix)
                                   (string-append prefix "  ")))
                             subcmds))
                  "\n\n")))))))


;;;;; Helpers

(define (keywords->setting-specs keywords)
  (filter-map (match-lambda
                ((? setting? n)
                 `((name . ,(setting-name n))
                   (help . ,(match (setting-description n)
                              ((? string-null?) (setting-synopsis n))
                              (desc desc)))
                   (value . ,(setting-default n))))
                (_ #f))
              keywords))

(define (write-codex parser path features valus)
  (apply parser-write parser
         (parser-file parser path (features-name features))
         (features-name features)
         (features-description features)
         (features-synopsis features)
         (filter setting? (valus-keywords valus))))

(define (sort-subcommands subcommands)
  "Return a formatted string consisting of the name and terse description of
the subcommands contained in CONFIGS."
  ;; Subcommand listing should be:
  ;; Subcommands:
  ;;   command1        command-terse
  ;;   command2        command-terse
  ;;   command3        command-terse
  ;; [2 spaces][padded name longest][ | padded alias longest-alias]terse
  (define (subcommand-spec name alias synopsis subcommands longest
                           longest-alias)
    ;; result: '(((subcommand-name alias synopsis) ...) longest longest-alias)
    (list (cons (list name alias synopsis) subcommands)
          longest
          (if ((compose (cut > <> longest-alias) string-length) alias)
              (string-length alias)
              longest-alias)))
  
  (string-join
   (match (fold (lambda (subcmd result)
                  (let ((string-name (symbol->string (configuration-name
                                                      subcmd)))
                        (string-alias (match (configuration-alias subcmd)
                                        (#f "")
                                        ((? symbol? alias)
                                         (symbol->string alias)))))
                    (match result
                      ((subcommands longest longest-alias)
                       (subcommand-spec
                        string-name string-alias
                        (configuration-synopsis subcmd)
                        subcommands
                        (if ((compose (cut > <> longest) string-length)
                             string-name)
                            (string-length string-name)
                            longest)
                        longest-alias)))))
                '(() 0 0)
                subcommands)
     ((subcommand-specs longest longest-alias)
      (sort
       (map (match-lambda
              ((name alias synopsis)
               (string-append "  " (padded name longest)
                              (match alias
                                (#f (string-append "   "
                                                   (padded "" longest-alias)))
                                (a (string-append (if (string-null? a)
                                                      "   "
                                                      " | ")
                                                  (padded a longest-alias))))
                              "  "
                              synopsis)))
            subcommand-specs)
       string-ci<=?))
     (n (throw 'sort-subcommands "no matching pattern" n)))
   "\n"))

(define (sort-detailed-keywords keywords)
  "Return a formatted string of KEYWORDS.  An example of our output:

    --name     -n   Name of user
    --target   -t   Target of game
    --zulu          Bogus option"
  (define (kwrd-spec name single-char terse keywords longest)
    ;; result: `(((name single-char terse) ...) . longest)
    (cons (cons (list name single-char terse) keywords)
          longest))

  (string-join
   (match (fold (lambda (kwd result)
                  (match result
                    ((keywords . longest)
                     (match kwd
                       ((or ($ <switch> (= symbol->string n) _ _ _ _ t)
                            ($ <setting> (= symbol->string n) _ _ _ _ t))
                        (if ((compose (cut > <> longest) string-length) n)
                            (kwrd-spec n (keyword-character kwd)
                                       t keywords (string-length n))
                            (kwrd-spec n (keyword-character kwd)
                                       t keywords longest)))
                       (n (throw 'sort-detailed-keywords
                                 "no matching pattern" n))))))
                '(() . 0)
                keywords)
     ((kwrd-specs . longest)
      (sort (map (match-lambda
                   ((n s t)
                    (string-append "  --" (padded n longest)
                                   (if s
                                       (string-append "  -" (string s))
                                       "    ")
                                   "  " t)))
                 kwrd-specs)
            string-ci<=?))
     (n (throw 'sort-detailed-keywords "no matching pattern" n)))
   "\n"))

(define (sort-keywords keywords indent arguments)
  "Return a formatted string of KEYWORDS, INDENTED up to level INDENT.
This formatting is intended for the brief summary of our command."
  (define (whitespace)
    (let moar ((togo indent)
               (white ""))
      (if (> togo 0)
          (moar (1- togo) (string-append " " white))
          white)))
  (define (arguments-string)
    (match (fold (lambda (arg result)
                   (match result
                     ((str count)
                      `(,(string-append
                          str (if (argument-optional? arg) "[" "")
                          (if (string-null? (argument-example arg))
                              ((compose string-upcase symbol->string
                                        argument-name) arg)
                              (argument-example arg)))
                        ,(if (argument-optional? arg) (1+ count) count)))))
                 '("" 0) arguments)
      ((str count)
       (let lp ((count  count)
                (string str))
         (if (zero? count)
             string
             (lp (1- count) (string-append string "]")))))
      (n (throw 'arguments-string "no matching pattern" n))))
  (define (boolproc? proc) (eq? 'boolean? (procedure-name proc)))

  (string-join
   (filter (cut (negate string=?) "" <>)
           ((lambda (almost-sorted-sets)
              (match almost-sorted-sets
                ((short-bools long-bools short-rest long-rest)
                 (cons (match (apply string (sort short-bools char-ci<=?))
                         ((? (compose (cut > <> 0) string-length) sbs)
                          (string-append "[-" sbs "] " (arguments-string)))
                         (_ ""))
                       (map (cut string-join <>
                                 (string-append "\n" (whitespace)))
                            (map (cut sort <> string-ci<=?)
                                 (list long-bools short-rest long-rest)))))
                (n (throw 'sort-keywords "no matching pattern" n))))
            ;; Results in:
            ;; (list (list of chars) (formated long keywords)
            ;;       (formatted short rest keywords) (formatted long rest keywords))
            (fold (lambda (keyword sorted)
                    (match sorted
                      ((short-bools long-bools short-rest long-rest)
                       (match keyword
                         ((or ($ <switch> n _ t _ _ _ _ e)
                              ($ <setting> n _ t _ _ _ _ e))
                          (let ((n (symbol->string n))
                                (s (keyword-character keyword)))
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
                                                              (match e
                                                                ("" "")
                                                                (e (string-append " " e)))
                                                              "]")
                                               short-rest)
                                         long-rest))
                                  (else
                                   `(,short-bools
                                     ,long-bools
                                     ,short-rest
                                     ,(cons (string-append "[--" n "=" e "]")
                                            long-rest))))))
                         (_ (throw 'config
                                   "Should not have happened"))))
                      (n (throw 'sort-keywords "no matching pattern" n))))
                  '(() () () ())
                  keywords)))
   (string-append "\n" (whitespace))))
