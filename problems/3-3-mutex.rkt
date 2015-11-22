#lang racket/base
(require little-book-of-semaphores)

(define count (box 0))

(define mutex (make-semaphore 1))

(define-thread A
  (wait mutex)
  (incr count)
  (signal mutex))

(define-thread B
  (wait mutex)
  (incr count)
  (signal mutex))

(module+ main
  (run)
  (printf "Final count: ~a\n" (unbox count)))
