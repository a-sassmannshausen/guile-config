;;; IO Monad --- Monadic IO in GNU Guile
;;; Copyright © 2015 Alex Sassmannshausen <alex@pompo.co>
;;;
;;; This file is part of Monads.
;;;
;;; Monads is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Monads is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with glean; if not, contact:
;;;
;;; Free Software Foundation           Voice:  +1-617-542-5942
;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (monads io)
  #:use-module (monads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (run-io
            io-output-port
            io-input-port
            io-error-port
            io-close-input-port
            io-close-output-port
            io-close-error-port
            set-io-output-port
            set-io-output-file
            set-io-input-port
            set-io-input-file
            set-io-error-port
            set-io-error-file

            %io-monad
            io-return
            io-bind
            io-lift

            ioread-char
            iopeek-char
            iounread-char
            iounread-string
            iodrain-input
            ioport-column
            ioport-line
            ioset-port-column!
            ioread-line
            ioread-delimited
            ioread-string
            ioread

            ionewline
            iosimple-format
            iowrite-char
            ioforce-output
            iowrite-line
            iowrite
            iodisplay

            iomkdir-p

            with-prompt))

;;; Commentary:
;;;
;;; I want to provide monadic IO through this module.  Guile is capable of
;;; side-effectful IO, but it pollutes my programs and then my mind.  This
;;; module imposes discipline on IO operations.
;;;
;;; run-io: the privileged actor; code that finally instantiates all IO ops.
;;;
;;; io-output-port: return a monadic value which returns the current output
;;; port of the monadic value.
;;;
;;; set-io-output-port: return a monadic value which has its output-port set
;;; to the argument supplied, and which returns the old output-port.
;;;
;;; io-input-port: return a monadic value which returns the current input port
;;; of the monadic value.
;;;
;;; set-io-input-port: return a monadic value which has its input port set
;;; to the argument supplied, and which returns the old input port.
;;;
;;; io-lift: specialised lift, turning non-monadic procedures into
;;; io-monad procedures.
;;;
;;; io-return: turn a value into a monadic-io value.  That value will
;;; be VALUE wrapped together with an input and output port.
;;;
;;; io-bind: unpack a monadic-io value, pass it to an io-lifted
;;; procedure, and return the result of that procedure.
;;;
;;; Code:

;;;; Monad Accessories

(define* (run-io mvalue #:optional (input-port (current-input-port))
                 (output-port (current-output-port))
                 (error-port (current-error-port)))
  "Return the value resulting from running MVALUE, a monadic value in the
io-monad, either with INPUT-PORT,OUTPUT-PORT and ERROR-PORT or with the
default ports."
  (match (force (mvalue input-port output-port error-port))
    ((value in out err) value)))

(define (io-output-port)
  "Return, as monadic value, the output port within this io-monad chain."
  (lambda (input-port output-port error-port)
    (delay (list output-port input-port output-port error-port))))

(define (io-input-port)
  "Return, as monadic value, the input port within this io-monad chain."
  (lambda (input-port output-port error-port)
    (delay (list input-port input-port output-port error-port))))

(define (io-error-port)
  "Return, as monadic value, the error port within this io-monad chain."
  (lambda (input-port output-port error-port)
    (delay (list error-port input-port output-port error-port))))

(define (set-io-output-port port)
  "Set the output port within the io-monad context to PORT, and return the
previous output-port."
  (lambda (input-port output-port error-port)
    (delay (list output-port input-port port error-port))))

(define (set-io-input-port port)
  "Set the input port within the io-monad context to PORT, and return the
previous input-port."
  (lambda (input-port output-port error-port)
    (delay (list input-port port output-port error-port))))

(define (set-io-error-port port)
  "Set the error port within the io-monad context to PORT, and return the
previous error-port."
  (lambda (input-port output-port error-port)
    (delay (list error-port input-port output-port port))))

(define (set-io-input-file filename)
  "Set the input port within the io-monad context to the file pointed to by
FILENAME, and return the previous input port."
  (lambda (input-port output-port error-port)
    (delay (list input-port (open-input-file filename) output-port error-port))))

(define (io-close-input-port new-input-port)
  "Set the input port within the io-monad context NEW-INPUT-PORT after closing
the current input port."
  (lambda (input-port output-port error-port)
    (delay (begin (close-input-port input-port)
                  (list '() new-input-port output-port error-port)))))

(define (set-io-output-file filename)
  "Set the output port within the io-monad context to the file pointed to by
FILENAME, and return the previous output-port."
  (lambda (input-port output-port error-port)
    (delay (list output-port input-port (open-output-file filename) error-port))))

(define (io-close-output-port new-output-port)
  "Set the output port within the io-monad context to NEW-OUTPUT-PORT after
closing the current output port."
  (lambda (input-port output-port error-port)
    (delay (begin (close-output-port output-port)
                  (list '() input-port new-output-port error-port)))))

(define (set-io-error-file filename)
  "Set the error port within the io-monad context to the file pointed to by
FILENAME, and return the previous error port."
  (lambda (input-port output-port error-port)
    (delay (list error-port input-port output-port (open-output-file filename)))))

(define (io-close-error-port new-error-port)
  "Set the error port within the io-monad context NEW-ERROR-PORT after closing
the current error port."
  (lambda (input-port output-port error-port)
    (delay (begin (close-output-port error-port)
                  (list '() input-port output-port new-error-port)))))

(define (preserve-documentation original proc)
  "Return PROC with documentation taken from ORIGINAL."
  (set-object-property! proc 'documentation
                        (procedure-property original 'documentation))
  proc)


;;;; The Monad

;;;;; The bind op
;;;
;;; The bind op should unwrap monadic-io MVALUE, then pass it to MPROC
;;; and finally return the new monadic-io value generated by the
;;; application.
;;;
;;; The IO monad provides a means to chain IO.  All IO operations have
;;; the following type:
;;; output: -> (return '())
;;; input: -> (return (read n))
;;;
;;; It should be possible to use bind to directly chain output and
;;; input mvalues, even though chaining output into anything will
;;; simply result in the anything being called with '() as an
;;; argument.
;;;
;;; A monadic-io PROCEDURE is a procedure that promises to perform io
;;; operations, guaranteeing the above results. 

;;; Return an io-mvalue which consist of the evaluation of MVALUE in the
;;; monadic context, and the creation of a new mvalue by passing the result of
;;; that application to MPROC.

(define-inlinable (io-bind mvalue mproc)
  (lambda (input-port output-port error-port)
    (delay
      (match (force (mvalue input-port output-port error-port))
        ((value in out err)
         (force ((mproc value) in out err)))))))

;;;;; The return op
;;;
;;; The return op should return a monadic-io value based on VALUE.
;;; A monadic-io value is a value which wen resolved using the
;;; privileged actor `run-io' will perform IO operations using the
;;; input/output ports set as its context.
;;; As a consequence io-return needs to wrap value with its ports:
;;; they are the 'state' passed along implicitly in this monad.

;;; Return an io-mvalue which consists of VALUE.

(define-inlinable (io-return value)
  (lambda (input-port output-port error-port)
    (delay (list value input-port output-port error-port))))

;;; The monad.
(define-monad %io-monad
  (bind io-bind)
  (return io-return))

;;;;; IO Lift
;;;
;;; IO lift turns a normal procedure into a monadic-io procedure.  A
;;; monadic-io procedure is a procedure that simply promises to
;;; perform an io operation on its arguments.  In order to return the
;;; correct result as expected by other monadic-io procs, io-lift
;;; should be called with an argument specifying whether this is an
;;; input or output operation.

(define* (io-lift proc #:optional type-of-op invert?)
  "Return a monadic procedure derived from PROC, with it's documentation
preserved.  The returned procedure will guarantee to fulfil the IO-mval
contract by specifying 'input 'output or 'error as TYPE-OF-OP.  Normally, io
procedures take port as a last argument.  If INVERT? is #t then we make sure
we pass port as the first argument."
  (preserve-documentation
   proc (lambda args
          (define (perform port)
            (if invert?
                (apply proc port args)
                (apply proc (append args `(,port)))))

          (match type-of-op
            ('input
             (lambda (input-port output-port error-port)
               (delay
                 (list (perform input-port) input-port output-port
                       error-port))))
            ('output
             (lambda (input-port output-port error-port)
               (delay
                 (begin (perform output-port)
                        (list '() input-port output-port error-port)))))
            ('error
             (lambda (input-port output-port error-port)
               (delay
                 (begin (perform error-port)
                        (list '() input-port output-port error-port)))))
            (_ (with-monad %io-monad
                 (return (apply proc args))))))))


;;;; Monadic Procedures

;;;;; Reading

(define ioread-char (io-lift read-char 'input))

(define iopeek-char (io-lift peek-char 'input))

(define iounread-char (io-lift unread-char 'input))

(define iounread-string (io-lift unread-string 'input))

(define iodrain-input (io-lift drain-input 'input))

(define ioport-column (io-lift port-column 'input))

(define ioport-line (io-lift port-line 'input))

(define ioset-port-column! (io-lift set-port-column! 'input))

(define ioset-port-line! (io-lift set-port-line! 'input))

(define ioread-line (io-lift read-line 'input #t))

;;; Missing [handle-delims], because port is the middle argument!
(define ioread-delimited (io-lift read-delimited 'input))

(define ioread-string (io-lift read-string 'input #t))

(define ioread (io-lift read 'input))

;;;;; Writing

(define ionewline (io-lift newline 'output))

(define iosimple-format (io-lift simple-format 'output #t))

(define iowrite-char (io-lift write-char 'output))

(define ioforce-output (io-lift force-output 'output))

(define iowrite-line (io-lift write-line 'output))

(define iowrite (io-lift write 'output))

(define iodisplay (io-lift display 'output))

;;;;; Filesystem manipulation

(define iomkdir-p
  (io-lift
   (lambda (dir ignored)
     "Create directory DIR and all its ancestors as necessary."
     (define absolute?
       (string-prefix? "/" dir))

     (define not-slash
       (char-set-complement (char-set #\/)))

     (let loop ((components (string-tokenize dir not-slash))
                (root       (if absolute?
                                ""
                                ".")))
       (match components
         ((head tail ...)
          (let ((path (string-append root "/" head)))
            (catch 'system-error
              (lambda ()
                (mkdir path)
                (loop tail path))
              (lambda args
                (if (= EEXIST (system-error-errno args))
                    (loop tail path)
                    (apply throw args))))))
         (() #t))))
   'output))

;;;;; Mixed

(define (with-prompt input-mproc format-string . args)
  (mbegin %io-monad
    (apply iosimple-format format-string args)
    (input-mproc)))

;;; E.g.:
;;; (run-io ((with-prompt (io-lift read 'input)) "Please enter a s-exp:  "))
;;; (run-io ((with-prompt (io-lift read 'input)) "Please enter a s-exp:  ")
;;;          (open-input-string "(9 8 7 6 5 4 3 2 1)"))

;;; io.scm ends here
