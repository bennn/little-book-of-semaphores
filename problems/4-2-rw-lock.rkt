#lang racket/base
(require little-book-of-semaphores)

(require racket/class)

(define rw-lock%
  (class object%
    (super-new)
    (field
      [num-readers (box 0)]
      [num-writers (box 0)]
      [writer-gone (make-semaphore 0)]
      [readers-gone (make-semaphore 0)]
      [mutex (make-semaphore 1)])

    (define/public (reader-enter)
      (wait mutex)
      (let loop ()
        (unless (zero? (unbox num-writers))
          (signal mutex)
          (sleep 0.1) ;; nooo
          (wait mutex)))
      (incr num-readers)
      (signal mutex))

    (define/public (reader-exit)
      (wait mutex)
      (decr num-readers)
      (define nr (unbox num-readers))
      (signal mutex)
      (when (zero? nr)
        (signal readers-gone)))

    (define/public (writer-enter)
      (wait mutex)
      (let loop ()
        (unless (and (zero? (unbox num-readers))
                     (zero? (unbox num-writers)))
          (signal mutex)
          (sleep 0.1) ;; nooo
          (wait mutex)))
      (incr num-writers)
      (signal mutex))

    (define/public (writer-exit)
      (wait mutex)
      (decr num-writers)
      (signal mutex))))

;; -----------------------------------------------------------------------------

(module+ test
  (define lock (new rw-lock%))

  (define-syntax-rule (make-reader id* ...)
    (begin
      (define-thread id*
        (send lock reader-enter)
        (printf "~a reading\n" (object-name id*))
        (send lock reader-exit)) ...))

  (define-syntax-rule (make-writer id* ...)
    (begin
      (define-thread id*
        (send lock writer-enter)
        (printf "~a writing\n" (object-name id*))
        (send lock writer-exit)) ...))

  (make-reader R1 R2 R3 R4)
  (make-writer W1 W2)

  (run))

