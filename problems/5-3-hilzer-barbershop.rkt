#lang little-book-of-semaphores

;; TODO
;; - assumes FIFO semaphores
;; - barbers busy-wait

;; -----------------------------------------------------------------------------

(define MAX-OCCUPANCY 10)
(define NUM-CHAIRS 3)
(define NUM-SOFA 4)
(define NUM-STANDING (- MAX-OCCUPANCY NUM-CHAIRS NUM-SOFA))

;; FIFO on sofa, FIFO on standing room
(define occupy (make-semaphore MAX-OCCUPANCY))
(define sofa   (make-semaphore NUM-SOFA))
(define chair (make-semaphore NUM-CHAIRS))
(define haircut (make-semaphore 0))
(define cash-register (make-semaphore 0))

(define hair-ready (box 0))
(define hair-mutex (make-semaphore 1))

(define pay-ready (box 0))
(define pay-mutex (make-semaphore 1))

;; -----------------------------------------------------------------------------

(define (enter-shop id)
  (wait occupy)
  (printf "Customer ~a entered the shop\n" id))

(define (enter-sofa id)
  (wait sofa))

(define (enter-chair id)
  (wait chair)
  (printf "Customer ~a is waiting for a haircut\n" id)
  (signal sofa)
  (with hair-mutex (incr hair-ready))
  (wait haircut))

(define (pay id)
  (printf "Customer ~a got a haircut\n" id)
  (with pay-mutex (incr pay-ready))
  (wait cash-register)
  (printf "Customer ~a is paying\n" id)
  (signal chair))

(define (exit-shop id)
  (printf "Customer ~a says goodbye\n" id)
  (signal occupy))

(define (cut-hair id)
  (with hair-mutex
    (let ([hr (unbox hair-ready)])
      (if (zero? hr)
        #f
        (begin
          (printf "Barber ~a is cutting hair\n" id)
          (decr hair-ready)
          (signal haircut)
          #t)))))

(define (accept-payment id)
  (with pay-mutex
    (let ([pr (unbox pay-ready)])
      (if (zero? pr)
        #f
        (begin
          (printf "Barber ~a accepted payment\n" id)
          (decr pay-ready)
          (signal cash-register)
          #t)))))

;; -----------------------------------------------------------------------------

(define-syntax-rule (make-customer* id* ...)
  (begin
    (define-thread id*
      (let ([name (object-name id*)])
        (forever
          (enter-shop  name)
          (enter-sofa  name)
          (enter-chair name)
          (pay         name)
          (exit-shop   name)))) ...))

(define-syntax-rule (make-barber* id* ...)
  (begin
    (define-thread id*
      (let ([name (object-name id*)])
        (forever
          (if (zero? (random 2))
            (cut-hair name)
            (accept-payment name))))) ...))

;; -----------------------------------------------------------------------------

(define NUM-BARBERS 3)
(define NUM-CUSTOMERS 30)

(module+ test
  (make-barber* b1 b2 b3)
  ;(make-customer* c1 c2)
  (make-customer* c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17)
  (run))
