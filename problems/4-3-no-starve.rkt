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

(define no-starve-mutex2%
  (class object%
    (super-new)
    (field
     [mutex (make-semaphore 1)]
     [on-deck (make-semaphore 1)])

    (define/public (wait)
      (semaphore-wait on-deck)
      (semaphore-wait mutex)
      (semaphore-post on-deck))

    (define/public (signal)
      (semaphore-post mutex))))

;; Dude, this shit doesn't seem better than mine... 
(define no-starve-morris%
  (class* object% (lock<%>)
    (super-new)
    (field
     [room1 (box 0)]
     [room2 (box 0)]
     [mutex (make-semaphore 1)]
     [t1 (make-semaphore 1)]
     [t2 (make-semaphore 0)])

    ;; #too-hard!
    (define/public (wait)
      (semaphore-wait mutex)
      (incr room1)
      (semaphore-post mutex)
      (semaphore-wait t1)
      (incr room2)
      (semaphore-wait mutex)
      (decr room1)
      (if (zero? (unbox room1))
        (begin
          (semaphore-post mutex)
          (semaphore-post t2))
        (begin
          (semaphore-post mutex)
          (semaphore-post t1)))
      (semaphore-post t2)
      (decr room2))

    (define/public (signal)
      (if (zero? (unbox room2))
        (semaphore-post t1)
        (semaphore-post t2)))))

;; -----------------------------------------------------------------------------

;; Threads running in an infinite loop should eventually all go
(module+ test
  (define M (new no-starve-morris%))

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
