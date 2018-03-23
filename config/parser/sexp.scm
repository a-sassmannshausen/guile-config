;;; Config Parser Sexp --- Scheme read/write parser
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

(define-module (config parser sexp)
  #:use-module (config api)
  #:use-module (config helpers)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (sexp-parser))

;;;;; Parser Interface

;; Parsers should implement the following interface:
;;
;; - (parser-file parser <path> subcommand-name)
;;   => Return a path containing the complete filename of the configuration
;;      file at directory <path>.
;; - (parser-write file-path subcommand-name (or subcommand-description subcommand-synopsis) (settings ...))
;;   => Write settings to the configuration file at file-path.  If the
;;      configuration file already exists, do nothing.
;; - (parser-read file-path)
;;   => Return an association of form '((setting-name . setting-default) ...)
;;      read from the configuration file at file-path.  If the file does not
;;      exist, return '().
;;
;; Parsers can optionally implement the following:
;;
;; - (parser-write-complete configuration)
;;   => Write a complete representation of all the settings in the
;;      root-configuration configuration.  This is invoked prior to
;;      `parser-write'.
;;
;; If an optional implementation is present then it will be invoked at a
;; specified time.

(define (parser-file path subcommand-name)
  "Return the full filename for the subcommand SUBCOMMAND-NAME at <path>
PATH."
  (string-append (path-given path) (symbol->string subcommand-name)))

(define (parser-write file-path subcmd-name subcmd-desc subcmd-synopsis
                      . settings)
  "Write the configuration-file for the subcommand identified by SUBCMD-NAME,
with strings SUBCMD-DESC & SUBCMD-SYNOPSIS at file-path.  We don't know about
configurations or codexes, we simply write SETTINGS."
  (define (print-comment field)
    (format #t ";;~%")
    (for-each (cut format #t ";; ~a~%" <>)
              (string-split (fill-paragraph field 75) #\newline)))

  (when (and (not (null? settings)) (not (file-exists? file-path)))
    (mkdir-p (dirname file-path))
    (with-output-to-file file-path
      (lambda _
        (format #t ";;;; ~a~%" subcmd-name)
        (cond ((not (string-null? subcmd-desc))
               (print-comment subcmd-desc))
              ((not (string-null? subcmd-synopsis))
               (print-comment subcmd-synopsis)))
        (newline)
        (for-each (lambda args
                    (match args
                      ((name synopsis description example default)
                       (format #t ";;;;; ~a~%" name)
                       (cond ((not (string-null? description))
                              (print-comment description))
                             ((not (string-null? synopsis))
                              (print-comment synopsis)))
                       (when (not (string-null? example))
                         (format #t ";;~%;; Example: ~a~%" example))
                       (newline)
                       (pretty-print (cons name default))
                       (newline))
                      (_ 'unworkable-configuration)))
                  (map setting-name settings)
                  (map setting-synopsis settings)
                  (map setting-description settings)
                  (map setting-example settings)
                  (map setting-default settings))))))

(define (parser-read file-path)
  "Return an association-list of the settings contained in the
configuration-file at FILE-PATH."
  (catch #t
    (lambda _
      (with-input-from-file file-path
        (lambda _
          (let lp ((current (read))
                   (result '()))
            (if (eof-object? current)
                (reverse result)
                (lp (read)
                    (cons current result)))))))
    (lambda args
      '())))

;; Return the complete parser.
(define sexp-parser
  (make-parser parser-file parser-read parser-write #f))
