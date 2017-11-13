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

;;;;; The Sexp Parser

;; The following rules apply to this parser:
;; - It writes one configuration file for an entire configuration tree.
;;   + We use a nested association list for writing the file.
;;   + Each subcommand contains its own section in the file.
;;   + Inherited <settings> are repeated in each section that inherits them.
;; - It writes the same configuration file to each directory in the
;;   configuration and its subcommands.
;;   + The writer works best in applications where subcommands simply inherit
;;     the directory from root.
;;   + If you have different configuration directories, you may want to
;;     consider a parser that writes multiple configuration files.
;; - The reader simply reads the specific subcommand of the file.

(define (sexp-reader configuration)
  #t)

(define* (sexp-writer configuration #:optional port)
  (define with-output-proc
    (if (port? port)
        (cut with-output-to-port port <>)
        (cut with-output-to-file
             (string-append (match (configuration-directory configuration)
                              (($ <empty>)
                               (throw 'sexp-writer
                                      "No configuration directory specified"
                                      (configuration-name configuration)))
                              ((? string? s) s))
                            file-name-separator-string
                            "config.scm") <>)))
  (match (configuration-directory configuration)
    (($ <empty>) #t)
    ((? string? dirname)
     (mkdir-p (configuration-directory configuration))
     (with-output-proc
      (lambda ()
        (print
         (let lp ((forest (list configuration))
                  (rev-breadcrumbs '()))
           (fold (lambda (current result)
                   (append result
                           (let ((rev-breadcrumbs (cons (configuration-name current)
                                                        rev-breadcrumbs)))
                             (cons `((breadcrumbs . ,(reverse rev-breadcrumbs))
                                     (help . ,(match (configuration-description current)
                                                ((? string-null?)
                                                 (configuration-synopsis current))
                                                (desc desc)))
                                     (settings . ,(keywords->setting-specs
                                                   (configuration-keywords current))))

                                   (match (configuration-subcommands current)
                                     (() '())
                                     (subcommands
                                      (lp subcommands rev-breadcrumbs)))))))
                 '()
                 forest))))))))

(define sexp-parser
  (make-parser sexp-reader sexp-writer))

;;;; Helpers

(define* (print printables #:optional (port (current-output-port)))
  (pretty-print printables)
  (for-each
   (match-lambda
     ((('breadcrumbs . breadcrumbs)
       ('help . help)
       ('settings . (settings ...)))
      (begin
        (write breadcrumbs)
        (format port "~%")
        (for-each
         (lambda (line)
           (pretty-print line port
                         #:per-line-prefix ";; "
                         #:display? #t))
         (string-split (fill-paragraph help 68) #\newline))
        (format port "~%")
        (for-each (lambda (setting)
                    (match setting
                      ((('name . name)
                        ('help . help)
                        ('value . value))
                       (for-each
                        (lambda (line)
                          (pretty-print line port
                                        #:per-line-prefix ";;; "
                                        #:display? #t))
                        (string-split (fill-paragraph help 68)
                                      #\newline))
                       (pretty-print (cons name value) port #:width 72
                                     #:max-expr-width 72)
                       (format port "~%"))))
                  settings)))
     (printable
      (throw 'print "PRINTABLE does not fulfill contract." printable)))
   printables))

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
