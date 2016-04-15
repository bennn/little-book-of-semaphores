#lang little-book-of-semaphores

(define-for-syntax CAPACITY 5)

(define mutex (make-semaphore 1))
(define num-board (box 0))

(define all-aboard (make-semaphore 0))
(define all-offboard (make-semaphore 0))

(define can-board (make-semaphore 0))
(define can-offboard (make-semaphore 0))

;; -----------------------------------------------------------------------------

(define-syntax (define-car stx)
  (syntax-parse stx
   [(_)
    (quasisyntax/loc stx
      (define-thread car
        (forever
          (printf "CAR is warming up...\n")
          (sleep 1)
          (printf "CAR is ready to board\n")
          (for ([i (in-range '#,CAPACITY)])
            (signal can-board))
          (wait all-aboard)
          (printf "CAR is cruising\n")
          (sleep 1)
          (for ([i (in-range '#,CAPACITY)])
            (signal can-offboard))
          (wait all-offboard))))]))

(define-syntax (define-passenger stx)
  (syntax-parse stx
   [(_ ?i:nat)
    #:with name (format-id stx "p~a" (syntax-e #'?i))
    (quasisyntax/loc stx
      (define-thread name
        (forever
          (wait can-board)
          (with mutex
            (incr num-board)
            (printf "~a is in the car\n" 'name)
            (when (= '#,CAPACITY (unbox num-board))
              (signal all-aboard)))
          (wait can-offboard)
          (printf "~a is off the car\n" 'name)
          (with mutex
            (decr num-board)
            (when (= '0 (unbox num-board))
              (signal all-offboard))))))]))

(define-syntax (define-passengers stx)
  (syntax-parse stx
   [(_ ?n:nat)
    (define N (syntax-e #'?n))
    (unless (< CAPACITY N)
      (raise-user-error 'invariant "Must have more passengers than capacity (~a)" CAPACITY))
    (define p*
      (for/list ([n (in-range N)])
        (quasisyntax/loc stx (define-passenger #,n))))
    (quasisyntax/loc stx (begin #,@p*))]))

;; =============================================================================

(module+ test
  (define-passengers 17)
  (define-car)
  (run)
)
