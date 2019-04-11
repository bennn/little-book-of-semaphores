#lang little-book-of-semaphores

;; TODO generalize to multiple cars
;; - 1 car boards at a time
;; - multiple cars run concurrently
;; - cars must unload in order

(define-for-syntax CAPACITY 5)

;; TODO generalize these, but keep same passenger interface
(define mutex (make-semaphore 1))
(define num-board (box 0))

;; TODO what should these be?
(define can-board #f)
(define all-aboard #f)
(define can-offboard #f)
(define all-offboard #f)

;; -----------------------------------------------------------------------------

(define load-mutex (make-semaphore 1))
(define load-queue (box '()))

(define-syntax-rule (loading id e* ...)
  (with load-mutex
    (set-box! load-queue (cons id (unbox load-queue)))
    e* ...))

(define (spinlock e #:wait m #:cond b)
  (wait m)
  (let loop ()
    (if (b)
      (begin (e) (signal m))
      (begin (signal m) (sleep 1) (wait m) (loop)))))

;; spinlock
(define-syntax-rule (unloading id e* ...)
  (spinlock
    #:wait load-mutex
    #:cond (lambda () (eq? id (car (unbox load-queue))))
    (lambda () e* ...)))

(define-syntax (define-cars stx)
  (syntax-parse stx
   [(_ NUM-CAR:nat)
    #:with (car-id* ...)
           (for/list ([i (in-range (syntax-e #'NUM-CAR))])
             (format-id stx "car~a" i))
    (quasisyntax/loc stx
     (begin
      (define-thread car-id*
        (forever
          (printf "~a is warming up...\n" 'car-id*)
          (sleep 1)
          (printf "~a is ready to board\n" 'car-id*)
          (loading 'car-id*
            (for ([i (in-range '#,CAPACITY)])
              (signal can-board))
            (wait all-aboard))
          (printf "~a is cruising\n" 'car-id*)
          (sleep 1)
          (unloading 'car-id*
            (for ([i (in-range '#,CAPACITY)])
              (signal can-offboard))
            (wait all-offboard)))) ...))]))

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
  (define-cars 4)
  (run)
)
