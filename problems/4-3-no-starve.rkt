#lang little-book-of-semaphores

;; Assuming weak semaphores & finitely-many threads, implement a no-starve mutex
;;   (weak = number of threads woken before thread T wakes up is bounded)

(provide
)

(require "3-8-fifo.rkt")

;; -----------------------------------------------------------------------------

;; TODO check if queue is non-empty and T was last thread to go
(define no-starve-mutex%
  (class object%
    (super-new)
    (field
      [mutex (make-semaphore 1)]
      [free? (box #t)]
      [Q (new fifo%)])

    (define/public (wait)
      (with mutex
        (let loop ()
          (unless (unbox free?)
            (semaphore-post mutex)
            (send Q push)
            (semaphore-wait mutex)
            (loop)))
        (set-box! free? #f)))

    (define/public (signal)
      (with mutex
        (set-box! free? #t))
      (send Q pop))
))

;; Threads running in an infinite loop should eventually all go
(module+ test
  (define M (new no-starve-mutex%))

  (define-syntax-rule (make-thread id* ...)
    (begin
      (define-thread id*
        (let loop ()
          (send M wait)
          (printf "Thread ~a is critical\n" (object-name id*))
          (send M signal)
          (loop))) ...))

  (make-thread A B C D)
  (run)
)
