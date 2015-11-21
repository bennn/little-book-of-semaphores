#lang racket/base
(require little-book-of-semaphores)

(define count 0)

(define mutex (make-semaphore 1))

(define-thread A
  (semaphore-wait mutex)
  (set! count (+ count 1))
  (semaphore-post mutex))

(define-thread B
  (semaphore-wait mutex)
  (set! count (+ count 1))
  (semaphore-post mutex))

(module+ main
  (run)
  (printf "Final count: ~a\n" count))
