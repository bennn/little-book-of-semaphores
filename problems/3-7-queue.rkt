#lang racket/base
(require little-book-of-semaphores)

;; TODO when could the queue ever go positive?

(define *num-leaders* (box 0))
(define *num-followers* (box 0))
(define mutex (make-semaphore 1))
(define leader-queue (make-semaphore 0))
(define follower-queue (make-semaphore 0))
(define done-dancing (make-semaphore 0))

(define-syntax-rule (make-leader id)
  (define-thread id
    (printf "leader ~a is ready\n" (object-name id))
    (wait mutex)
    (if (not (zero? (unbox *num-followers*)))
      (begin (decr *num-followers*)
             (signal follower-queue))
      (begin (incr *num-leaders*)
             (signal mutex)
             (wait leader-queue)))
    (printf "leader ~a is dancing\n" (object-name id))
    (wait done-dancing)
    (signal mutex)
    ;; -- not cool...
    (printf "leader ~a is ready\n" (object-name id))
    (wait mutex)
    (if (not (zero? (unbox *num-followers*)))
      (begin (decr *num-followers*)
             (signal follower-queue))
      (begin (incr *num-leaders*)
             (signal mutex)
             (wait leader-queue)))
    (printf "leader ~a is dancing\n" (object-name id))
    (wait done-dancing)
    (signal mutex)
    ))

(define-syntax-rule (make-follower id)
  (define-thread id
    (printf "follower ~a is ready\n" (object-name id))
    (wait mutex)
    (if (not (zero? (unbox *num-leaders*)))
      (begin (decr *num-leaders*)
             (signal leader-queue))
      (begin (incr *num-followers*)
             (signal mutex)
             (wait follower-queue)))
    (printf "follower ~a is dancing\n" (object-name id))
    (signal done-dancing)))

;; -----------------------------------------------------------------------------

(make-leader L1)
(make-leader L2)
(make-leader L3)

(make-follower F1)
(make-follower F2)
(make-follower F3)
(make-follower F4)
(make-follower F5)
(make-follower F6)

(module+ main
  (run))
