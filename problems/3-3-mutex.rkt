#lang racket/base
(require little-book-of-semaphores)

(define count 0)

(define mutex (make-semaphore 1))

(define-thread A
  (wait mutex)
  (set! count (+ count 1))
  (signal mutex))

(define-thread B
  (wait mutex)
  (set! count (+ count 1))
  (signal mutex))

(module+ main
  (run)
  (printf "Final count: ~a\n" count))
