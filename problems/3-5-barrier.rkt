#lang racket/base
(require little-book-of-semaphores)
(require (for-syntax syntax/parse racket/base racket/syntax))

;; Goal: all threads rendezvous, then do critical
(define *num-threads* (box 0))

(define can-enter? (make-semaphore 0))
(define mutex (make-semaphore 1))

(define (rendezvous id)
  (printf "~a : rendezvous\n" id)
  (wait mutex)
  (decr *num-threads*)
  (define nt (unbox *num-threads*))
  (signal mutex)
  (if (zero? nt)
    (signal can-enter?)
    (wait can-enter?)))

(define (critical id)
  (signal can-enter?)
  (printf "~a : critical\n" id))

(define-syntax-rule (make-thread* id* ...)
  (begin
    (define-thread id*
      (rendezvous (object-name id*))
      (critical (object-name id*))) ...))

(module+ main
  ;; TODO loose connection between `4` and `A B C D`
  (set-box! *num-threads* 4)
  (make-thread* A B C D)
  (run))

