#lang little-book-of-semaphores

(require (for-syntax racket/base syntax/parse racket/syntax))

;; -----------------------------------------------------------------------------

(define M 5)
(define pot (box 0))

;; Constraints:
;; - Cook may not fill non-empty pot
;; - Savages may not eat from empty pot

(define mutex (make-semaphore 1))
(define pot-full (make-semaphore 0))
(define pot-empty (make-semaphore 0))

(define (get-pot)
  (with mutex
    (let loop ()
      (when (zero? (unbox pot))
        (signal mutex)
        (wait pot-full)
        (wait mutex)
        (loop)))
    (decr pot)
    (if (zero? (unbox pot))
      (signal pot-empty)
      (signal pot-full))))

(define (fill-pot)
  (with mutex
    (let loop ()
      (unless (zero? (unbox pot))
        (signal mutex)
        (wait pot-empty)
        (wait mutex)
        (loop)))
    (set-box! pot M)
    (signal pot-full))
  (printf "ORDER UP\n"))

(define (eat id)
  (printf "~a is eating\n" id)
  (sleep (random)))

;; -----------------------------------------------------------------------------

(define-for-syntax NUM-SAVAGES (box 0))

(define-syntax make-savage
  (syntax-parser
   [stx
    #:when (set-box! NUM-SAVAGES (add1 (unbox NUM-SAVAGES)))
    #:with name (format-id #'stx "Savage-~a" (unbox NUM-SAVAGES))
    #`(define-thread name
        (forever
          (get-pot)
          (eat '#,(syntax-e #'name))))]))

(define-thread Cook
  (forever
    (fill-pot)
    (sleep (random))))

;; -----------------------------------------------------------------------------

(module+ test
  (make-savage)
  (make-savage)
  (make-savage)
  (make-savage)
  (run))

