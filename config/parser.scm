;;; Config Parser --- A simple scheme config format
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

(define-module (config parser)
  #:use-module (config spec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 streams)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (
            make-configuration-parser
            configuration-parser?
            configuration-parser-reader
            configuration-parser-writer

            simple-parser
            fill-paragraph
            ))


;;;; Configuration Parsers
;;;
;;; Configuration parsers are simple records consisting of a "reader" and a
;;; "writer".  Each of these should be procedures of 2 arguments, a
;;; <configuration> and a port.  The writer should emit the options of
;;; <configuration> to port.  It's return is ignored.  The reader should merge
;;; the contents of port with <configuration> and return that merged
;;; <configuration>.

(define-record-type <configuration-parser>
  (make-configuration-parser reader writer)
  configuration-parser?
  (reader configuration-parser-reader)
  (writer configuration-parser-writer))


;;;; Parsers

;;;;; The Simple Parser
;;;
;;; The simple parser writes configuration options as simple pairs to the
;;; configuration file, prepended by that option's terse and long as
;;; commentary. 

(define simple-parser
  (make-configuration-parser
   ;; Reader
   (lambda (configuration port)
     (set-configuration-values
      configuration
      (map ((lambda (opts)
              (lambda (opt)
                (match opt
                  ((name . (? open-option? opt))
                   (let ((new (assq-ref opts name)))
                     (if ((openoption-test opt) new)
                         (cons name (set-openoption-value opt new))
                         (begin
                           (format (current-error-port)
                                   "Configuration predicate failed: ~a [~s]~%"
                                   name new)
                           (exit 1)))))
                  (_ opt))))
            (stream->list (port->stream port read)))
           (configuration-values configuration))))
   ;; Writer
   (lambda (configuration port)
     (match configuration
       (($ <configuration> name _ values terse long _)
        (format port ";;;; ~a - ~a~%" name terse)
        (when long
          (for-each (lambda (line)
                      (pretty-print line port #:per-line-prefix ";;; "
                                    #:display? #t))
                    (cons "" (string-split (fill-paragraph long 68)
                                           #\newline))))
        (format port "~%")
        (for-each (lambda (value)
                    (match value
                      ((name . ($ <openoption> name value _ _ _ terse long))
                       (format port ";;;; ~a~%;;;~%" name)
                       (for-each (lambda (line)
                                   (pretty-print line port
                                                 #:per-line-prefix ";;; "
                                                 #:display? #t))
                                 (string-split (fill-paragraph terse 68)
                                               #\newline))
                       (when long
                         (for-each
                          (lambda (line)
                            (pretty-print line port
                                          #:per-line-prefix ";;; "
                                          #:display? #t))
                          (cons "" (string-split (fill-paragraph long 68)
                                                 #\newline))))

                       (format port "~%")
                       (pretty-print (cons name value) port #:width 72
                                     #:max-expr-width 72)
                       (format port "~%"))
                      (_ #f)))
                  (sort values
                        (lambda (a b)
                          (string-ci<? (symbol->string (car a))
                                       (symbol->string (car b)))))))))))


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

;;; parser ends here
