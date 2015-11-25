#lang racket/base
(require little-book-of-semaphores)
(require racket/class)

(provide bounded-buffer%)

;; -----------------------------------------------------------------------------

(define bounded-buffer%
  (class object%
    (super-new)
    (init-field
      limit) ;; Natural
    (field
      [B '()]
      [size (box 0)]
      [item-added (make-semaphore 0)]
      [item-removed (make-semaphore 0)]
      [mutex (make-semaphore 1)])

    (define/public (push event)
      (wait mutex)
      (let loop ()
        (when (= (unbox size) limit)
          (signal mutex)
          (wait item-removed)
          (wait mutex)
          (loop)))
      (incr size)
      (set-field! B this (cons event B))
      (signal mutex)
      (signal item-added))

    (define/public (pop)
      (wait mutex)
      (let loop ()
        (when (zero? (unbox size))
          (signal mutex)
          (wait item-added)
          (wait mutex)
          (loop)))
      (define v (car B))
      (set-field! B this (cdr B))
      (signal item-removed)
      (decr size)
      (signal mutex)
      v)))

;; -----------------------------------------------------------------------------

(module+ test

  (define buffer (new bounded-buffer% [limit 2]))

  (define (poll)
    (sleep (random))
    (gensym 'event))

  (define (respond e)
    (sleep 1)
    (printf "Handled ~a\n" e))


  (define-syntax-rule (make-producer id* ...)
    (begin
      (define-thread id*
        (let loop ()
          (define event (poll))
          (send buffer push event)
          (printf "Pushed ~a\n" event)
          (sleep (random))
          (loop))) ...))

  (define-syntax-rule (make-consumer id* ...)
    (begin
      (define-thread id*
        (sleep 4)
        (let loop ()
          (define event (send buffer pop))
          (respond event)
          (loop))) ...))

  (make-consumer C1)
  (make-producer P1 P2 P3 P4)
  (run))
