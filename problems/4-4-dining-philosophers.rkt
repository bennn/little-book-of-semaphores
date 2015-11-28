#lang little-book-of-semaphores

(require (for-syntax racket/base syntax/parse racket/syntax))

;; Dining philosophers
;; - No sharing forks
;; - No deadlock
;; - No starvation
;; - Yes simultaneous eating

;; -----------------------------------------------------------------------------

(define NUM-PHILOSOPHERS 5)
(define NUM-FORKS NUM-PHILOSOPHERS)

;; Return this fork's successor
(define (next-fork id)
  (modulo (+ 1 id) NUM-FORKS))

(define fork*
  (for/vector ([i (in-range NUM-FORKS)])
    (make-semaphore 1)))

(define (fork-wait i)
  (wait (vector-ref fork* i)))

(define (fork-signal i)
  (signal (vector-ref fork* i)))

;; -----------------------------------------------------------------------------

(define (think id)
  (printf "Philosopher ~a is thinking...\n" id)
  (sleep (random)))

;; Invariant: grab the even-numbered fork
(define (get-forks id)
  (define f1 id)
  (define f2 (next-fork id))
  (if (even? f1)
    (begin (fork-wait f1)
           (fork-wait f2))
    (begin (fork-wait f2)
           (fork-wait f1))))

(define (eat id)
  (printf "Philosopher ~a is eating\n" id)
  (sleep (random)))

(define (put-forks id)
  (fork-signal id)
  (fork-signal (next-fork id)))

;; -----------------------------------------------------------------------------

(define-syntax make-philosopher
  (syntax-parser
   [(_ n:nat)
    #:with id (format-id #'n "p~a" (syntax-e #'n))
    #'(define-thread id
        (forever
          (think n)
          (get-forks n)
          (eat n)
          (put-forks n)))]))

;; -----------------------------------------------------------------------------

(module+ test
  (make-philosopher 0)
  (make-philosopher 1)
  (make-philosopher 2)
  (make-philosopher 3)
  (make-philosopher 4)
  ;; TODO automate philosopher-making
  (run))
