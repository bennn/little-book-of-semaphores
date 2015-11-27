#lang little-book-of-semaphores

;; Resuable barrier

(define *count-r* (box 0))
(define *count-c* (box 0))

(define can-enter? (make-semaphore 0))
(define can-leave? (make-semaphore 1))
(define mutex (make-semaphore 1))

(define (decr/mutex b)
  (wait mutex)
  (decr b)
  (define r (unbox b))
  (signal mutex)
  r)

(define (incr/mutex b)
  (wait mutex)
  (incr b)
  (define r (unbox b))
  (signal mutex)
  r)

;; -----------------------------------------------------------------------------

(define (rendezvous id)
  (printf "~a : rendezvous\n" id)
  (define o (decr/mutex *count-r*))
  (if (zero? o)
    (begin (wait can-leave?)
           (signal can-enter?))
    (begin (wait can-enter?)
           (signal can-enter?)))
  (incr/mutex *count-r*))

(define (critical id)
  (printf "~a : critical\n" id)
  (define i (decr/mutex *count-c*))
  (if (zero? i)
    (begin (wait can-enter?)
           (signal can-leave?))
    (begin (wait can-leave?)
           (signal can-leave?)))
  (incr/mutex *count-c*))

;; -----------------------------------------------------------------------------

(define-syntax-rule (make-thread id)
  (begin
    (incr *count-r*)
    (incr *count-c*)
    (define-thread id
      (rendezvous (object-name id))
      (critical (object-name id)))))

(make-thread A)
(make-thread B)
(make-thread C)
(make-thread D)
(make-thread E)

(module+ main
  (define NITERS 3)
  (for ([_i (in-range NITERS)])
    (run)))

