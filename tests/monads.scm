;;; Test Monads --- Monads in GNU Guile
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-monads)
  #:use-module (monads)
  #:use-module (monads io)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

;; Test the (monads) module.

(define %monads
  (list %identity-monad %state-monad %io-monad))

(define %monad-run
  (list identity
        (cut run-with-state <> '())
        run-io))

(define-syntax-rule (values->list exp)
  (call-with-values (lambda () exp)
    list))


(test-begin "monads")

(test-assert "monad?"
  (and (every monad? %monads)
       (every (compose procedure? monad-bind) %monads)
       (every (compose procedure? monad-return) %monads)))

;; The 3 "monad laws": <http://www.haskell.org/haskellwiki/Monad_laws>.

(test-assert "left identity"
  (every (lambda (monad run)
           (let ((number (random 777)))
             (with-monad monad
               (define (f x)
                 (return (* (1+ number) 2)))

               (= (run (>>= (return number) f))
                  (run (f number))))))
         %monads
         %monad-run))

(test-assert "right identity"
  (every (lambda (monad run)
           (with-monad monad
             (let ((number (return (random 777))))
               (= (run (>>= number return))
                  (run number)))))
         %monads
         %monad-run))

(test-assert "associativity"
  (every (lambda (monad run)
           (with-monad monad
             (define (f x)
               (return (+ 1 x)))
             (define (g x)
               (return (* 2 x)))

             (let ((number (return (random 777))))
               (= (run (>>= (>>= number f) g))
                  (run (>>= number (lambda (x) (>>= (f x) g))))))))
         %monads
         %monad-run))

(test-assert "lift"
  (every (lambda (monad run)
           (let ((f (lift1 1+ monad)))
             (with-monad monad
               (let ((number (random 777)))
                 (= (run (>>= (return number) f))
                    (1+ number))))))
         %monads
         %monad-run))

(test-assert ">>= with more than two arguments"
  (every (lambda (monad run)
           (let ((1+ (lift1 1+ monad))
                 (2* (lift1 (cut * 2 <>) monad)))
             (with-monad monad
               (let ((number (random 777)))
                 (= (run (>>= (return number)
                              1+ 1+ 1+
                              2* 2* 2*))
                    (* 8 (+ number 3)))))))
         %monads
         %monad-run))

(test-assert "mbegin"
  (every (lambda (monad run)
           (with-monad monad
             (let* ((been-there? #f)
                    (number (mbegin monad
                              (return 1)
                              (begin
                                (set! been-there? #t)
                                (return 2))
                              (return 3))))
               (and (= (run number) 3)
                    been-there?))))
         %monads
         %monad-run))

(test-assert "mapm"
  (every (lambda (monad run)
           (with-monad monad
             (equal? (run (mapm monad (lift1 1+ monad) (iota 10)))
                     (map 1+ (iota 10)))))
         %monads
         %monad-run))

(test-assert "sequence"
  (every (lambda (monad run)
           (let* ((input (iota 100))
                  (order '()))
             (define (frob i)
               (mlet monad ((foo (return 'foo)))
                 ;; The side effect here is used to keep track of the order in
                 ;; which monadic values are bound.  Perform the side effect
                 ;; within a '>>=' so that it is performed when the return
                 ;; value is actually bound.
                 (set! order (cons i order))
                 (return i)))

             (and (equal? input
                          (run (sequence monad (map frob input))))

                  ;; Make sure this is from left to right.
                  (equal? order (reverse input)))))
         %monads
         %monad-run))

(test-assert "listm"
  (every (lambda (monad run)
           (run (with-monad monad
                  (let ((lst (listm monad
                                    (return 1) (return 2) (return 3))))
                    (mlet monad ((lst lst))
                      (return (equal? '(1 2 3) lst)))))))
         %monads
         %monad-run))

(test-assert "anym"
  (every (lambda (monad run)
           (eq? (run (with-monad monad
                       (anym monad
                             (lift1 (lambda (x)
                                      (and (odd? x) 'odd!))
                                    monad)
                             (append (make-list 1000 0)
                                     (list 1 2)))))
                'odd!))
         %monads
         %monad-run))

(test-equal "set-current-state"
  (list '(a a d) 'd)
  (values->list
   (run-with-state
       (mlet* %state-monad ((init  (current-state))
                            (init2 (set-current-state 'b)))
         (mbegin %state-monad
           (set-current-state 'c)
           (set-current-state 'd)
           (mlet %state-monad ((last (current-state)))
             (return (list init init2 last)))))
     'a)))

(test-equal "state-push etc."
  (list '((z . 2) (p . (1)) (a . (1))) '(2 1))
  (values->list
   (run-with-state
       (mbegin %state-monad
         (state-push 1)    ;(1)
         (state-push 2)    ;(2 1)
         (mlet* %state-monad ((z (state-pop))        ;(1)
                              (p (current-state))
                              (a (state-push z)))    ;(2 1)
           (return `((z . ,z) (p . ,p) (a . ,a)))))
     '())))

(test-end "monads")


;;(exit (= (test-runner-fail-count (test-runner-current)) 0))

