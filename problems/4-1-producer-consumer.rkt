#lang racket/base
(require little-book-of-semaphores)
(require racket/class)

;; -----------------------------------------------------------------------------

(define (poll)
  (sleep (random))
  (gensym 'event))

(define (respond e)
  (printf "Handled ~a\n" e))

(define buffer%
  (class object%
    (super-new)
    (field
      [B '()]
      [size (box 0)]
      [item-added (make-semaphore 0)]
      [mutex (make-semaphore 1)])

    (define/public (push event)
      (wait mutex)
      (printf "Pushing ~a\n" event)
      (set-field! B this (cons event B))
      (incr size)
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
      (decr size)
      (signal mutex)
      v)))

;; -----------------------------------------------------------------------------

(define buffer (new buffer%))

(define-syntax-rule (make-producer id* ...)
  (begin
    (define-thread id*
      (let loop ()
        (define event (poll))
        (send buffer push event)
        (sleep (random))
        (loop))) ...))

(define-syntax-rule (make-consumer id* ...)
  (begin
    (define-thread id*
      (let loop ()
        (define event (send buffer pop))
        (respond event)
        (loop))) ...))

;; -----------------------------------------------------------------------------

(module+ main
  (make-consumer C1 C2)
  (make-producer P1 P2 P3 P4)
  (run))
