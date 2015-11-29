#lang little-book-of-semaphores

;; One agent, three smokers
;; - smokers need paper+tobacco+matches, then make & smoke
;; - each smoker has one ingredient
;; - agent has all ingredients, makes 2 random ones available at a time

;; This is the generalized solution:
;;  agents keep making ingredients, without waiting for smokers to use them

(require (for-syntax racket/base syntax/parse racket/syntax))

;; -----------------------------------------------------------------------------
;; Agent

(define agent-sem (make-semaphore 1))
(define match (make-semaphore 0))
(define paper (make-semaphore 0))
(define tobacco (make-semaphore 0))
(define ingredient* (vector match paper tobacco))

(define-syntax-rule (ingredient-wait i)
  (semaphore-wait (vector-ref ingredient* i)))

(define-thread Controller
  (forever
    (sleep (random))
    (signal agent-sem)))

(define-syntax make-agent
  (syntax-parser
   [(_ i1:id i2:id)
    #:with name (gensym 'Agent)
    #:with i1-sym (syntax-e #'i1)
    #:with i2-sym (syntax-e #'i2)
    #'(define-thread name
        (forever
          (wait agent-sem)
          (printf "Making ~a and ~a\n" 'i1-sym 'i2-sym)
          (signal i1)
          (signal i2)))]))

(make-agent tobacco paper)
(make-agent paper match)
(make-agent tobacco match)

;; -----------------------------------------------------------------------------

(define mutex (make-semaphore 1))
(define tobacco+match (make-semaphore 0))
(define match+paper   (make-semaphore 0))
(define paper+tobacco (make-semaphore 0))

;; Match Paper Tobacco
(define pouch (vector 0 0 0))
(define sema* (vector tobacco+match match+paper paper+tobacco))

(define-syntax-rule (pouch-signal i)
  (semaphore-post (vector-ref sema* i)))

(define-syntax-rule (pouch-wait i)
  (semaphore-wait (vector-ref sema* i)))

(define-syntax-rule (pouch-incr i)
  (vector-set! pouch i (add1 (vector-ref pouch i))))

(define-syntax-rule (pouch-decr i)
  (vector-set! pouch i (sub1 (vector-ref pouch i))))

(define-syntax-rule (nonzero? i)
  (not (zero? (vector-ref pouch i))))

(define (try-signal-both i #:phase b)
  (define i+ (modulo (add1 i) 3))
  (define i- (modulo (sub1 i) 3))
  (cond [b    (try-signal i i+)
              (try-signal i- i)]
        [else (try-signal i- i)
              (try-signal i i+)]))

(define (try-signal lo hi)
  (when (and (nonzero? lo) (nonzero? hi))
    (pouch-decr lo)
    (pouch-decr hi)
    (pouch-signal hi)))

(define-syntax make-pusher
  (syntax-parser
   [(_ ingredient:nat)
    #:with name (format-id #'ingredient "Pusher-~a" (syntax-e #'ingredient))
    #'(define-thread name
        (let loop ([phase #f])
          (ingredient-wait ingredient)
          (with mutex
            (pouch-incr ingredient)
            (try-signal-both ingredient #:phase phase))
          (loop (not phase))))]))

(make-pusher 0)
(make-pusher 1)
(make-pusher 2)

;; -----------------------------------------------------------------------------

(define (smoke msg)
  (displayln msg)
  (sleep 1))

(define-syntax make-smoker
  (syntax-parser
   [(_ i:nat)
    #:with name (format-id #'i "Smoker-~a" (syntax-e #'i))
    #`(define-thread name
        (forever
          (pouch-wait i)
          (smoke (format "~a is lit!" '#,(syntax-e #'name)))))]))

(make-smoker 0)
(make-smoker 1)
(make-smoker 2)

;; -----------------------------------------------------------------------------

(module+ test
  (run))
