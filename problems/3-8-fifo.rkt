#lang racket/base
(require little-book-of-semaphores)

(require racket/class)

(define fifo%
  (class object%
    (super-new)
    (field
      [mutex (make-semaphore 1)]
      [Q_in  '()]
      [Q_out '()])

    (define/public (push)
      (define s (make-semaphore 0))
      (with mutex
        (set-field! Q_in this (cons s (get-field Q_in this))))
      (wait s))

    (define/public (pop)
      (with mutex
        (when (null? (get-field Q_out this))
          (set-field! Q_out this (reverse (get-field Q_in this)))
          (set-field! Q_in this '()))
        (let ([s (car (get-field Q_out this))])
          (set-field! Q_out this (cdr (get-field Q_out this)))
          (signal s))))))

(module+ test
  (define fifo (make-object fifo%))

  (define-syntax-rule (make-thread id)
    (define-thread id
      (send fifo push)
      (printf "~a popped\n" (object-name id))))

  (make-thread A)
  (make-thread B)
  (make-thread C)
  (make-thread D)

  (define-thread F
    (sleep 0.5)
    (send fifo pop)
    (send fifo pop)
    (send fifo pop)
    (send fifo pop))

  (run))
