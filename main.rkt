#lang racket/base

;; Bindings to express & solve semaphore problems

;; TODO
;; - express constraints, random testing

(provide
  (all-from-out racket/base racket/class)
  ;; Essentials

  define-event define-event*
  ;; (define-event X)
  ;; Declares a callable event X. For use in later threads

  define-thread
  ;; (define-thread ID EXPR* ...)
  ;; Declares, but does not run, a thread named ID that should execute
  ;;  each EXPR* in sequence when run.

  lock<%>
  ;; Interface for locks:
  ;; Methods:
  ;; - wait
  ;;   aka lock
  ;; - signal
  ;;   aka unlock

  (struct-out abstract-lock)
  ;; Struct "interface" for locks
  ;; Fields are 'wait' and 'signal'

  repeat forever
  ;; Syntax for making loops.
  ;; `repeat n e* ...` does `e* ...` exactly `n` times.
  ;; `forever e* ...` loops indefinitely

  run
  ;; (run)
  ;; Runs all threads scheduled with `define-thread`

  wait signal
  ;; Semaphore operations, optionally take a natural number

  with
  ;; Shorthand for acquiring & releasing a semaphore

  incr decr
  ;; Syntax for boxes

  ;; --- Don't use these values!
  ;;     They are used internally by macros here
  thread* random-sleep
)

;; -----------------------------------------------------------------------------

(require
  racket/class
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(define thread* (box '()))

(define (random-sleep)
  (sleep 0.1))

(define-syntax define-event
  (syntax-parser
   [(_ s:id)
    #`(define s (lambda () (displayln #,(symbol->string (syntax-e #'s)))))]
   [stx
    (raise-user-error 'define-event (format "Expected a fresh identifier, got '~a'" (syntax->datum #'stx)))]))

(define-syntax-rule (define-event* s* ...)
  (begin (define-event s*) ...))

(define-syntax define-thread
  (syntax-parser
   [(_ name:id e*:expr ...)
    #'(begin
        (define name (lambda () (begin (begin e* (random-sleep)) ...)))
        (set-box! thread* (cons name (unbox thread*))))]
   [stx
    (raise-user-error 'define-thread (format "Expected (define-thread id expr* ...), got '~a'" #'stx))]))

(define-syntax-rule (repeat n e* ...)
  (for ([__i (in-range n)])
    (begin e* ...)))

(define-syntax-rule (forever e* ...)
  (let loop ()
    (begin e* ... (loop))))

(define-syntax run
  (syntax-parser
   [(_)
    #'(for-each thread-wait (map thread (unbox thread*)))]))

(define-syntax-rule (with s e* ...)
  (begin (wait s) e* ... (signal s)))

(define lock<%>
  (interface ()
    signal
    ;; Unlock

    wait
    ;; Lock
))

(struct abstract-lock (
  signal ;; (-> Void)
  wait   ;; (-> Void)
))

;; -----------------------------------------------------------------------------
;; -- Non-critical syntax

(define (wait S [N 1])
  (for ([_i (in-range N)])
    (semaphore-wait S)))

(define (signal S [N 1])
  (for ([_i (in-range N)])
    (semaphore-post S)))

(define-syntax-rule (incr b)
  (set-box! b (add1 (unbox b))))

(define-syntax-rule (decr b)
  (set-box! b (sub1 (unbox b))))
