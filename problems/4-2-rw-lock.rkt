#lang racket/base
(require little-book-of-semaphores)

(require racket/class)

(define rw-lock%
  (class object%
    (super-new)
    (field
      [num-readers (box 0)]
      [room-empty (make-semaphore 1)]
      [mutex (make-semaphore 1)])

    (define/public (reader-enter)
      (with mutex
        (when (zero? (unbox num-readers))
          (wait room-empty))
        (incr num-readers)))

    (define/public (reader-exit)
      (with mutex
        (decr num-readers)
        (when (zero? (unbox num-readers))
          (signal room-empty))))

    (define/public (writer-enter)
      (wait room-empty))

    (define/public (writer-exit)
      (signal room-empty))))

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
  (make-reader R5 R6 R7)
  (make-writer W3)
  (make-reader R8 R9 R10 R11 R12)

  (run))

