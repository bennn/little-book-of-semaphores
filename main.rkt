#lang racket/base

;; Bindings to express & solve semaphore problems

;; TODO
;; - this would be a great #lang
;; - express constraints, random testing

(provide
  define-event define-event*
  ;; (define-event X)
  ;; Declares a callable event X. For use in later threads

  define-thread
  ;; (define-thread ID EXPR* ...)
  ;; Declares, but does not run, a thread named ID that should execute
  ;;  each EXPR* in sequence when run.

  run
  ;; Runs all threads scheduled with `define-thread`

  thread*
  random-sleep
)

;; -----------------------------------------------------------------------------

(require
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
    (raise-user-error 'define-thread (format "Expected (define-thread id expr* ...), got '~a'" (syntax->datum #'stx)))]))

(define-syntax run
  (syntax-parser
   [(_)
    #'(for-each thread-wait (map thread (unbox thread*)))]))
