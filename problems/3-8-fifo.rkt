#lang little-book-of-semaphores

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
        (set! Q_in (cons s Q_in)))
      (wait s))

    (define/public (pop)
      (with mutex
        (when (null? Q_out)
          (set! Q_out (reverse Q_in))
          (set! Q_in '()))
        (let ([s (car Q_out)])
          (set! Q_out (cdr Q_out))
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
