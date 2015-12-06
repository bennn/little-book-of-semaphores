#lang little-book-of-semaphores

(define NUM-CHAIRS 6)
(define chair (make-semaphore NUM-CHAIRS))
(define customer-ready (make-semaphore 0))
(define done-cutting (make-semaphore 0))
(define customer-exit (make-semaphore 0))

(define (cut-hair)
  (printf "Barber is cutting hair\n"))

(define (get-haircut id)
  (printf "Customer ~a is getting a haircut\n" id))

;; -----------------------------------------------------------------------------

(define-thread barber
  (forever
    (wait customer-ready)
    (cut-hair)
    (signal done-cutting)
    (wait customer-exit)
    ))

(define-syntax-rule (make-customer* id* ...)
  (begin
    (define-thread id*
      (forever
        (wait chair)
        (signal customer-ready)
        (wait done-cutting)
        (get-haircut (object-name id*))
        (signal customer-exit)
        (signal chair))) ...))

;; -----------------------------------------------------------------------------

(module+ test
  (make-customer* A B C D E F G H I J K)
  (run)
)
