#lang racket/base

(provide
  define-event define-event*
  define-thread
  run
  thread*
)

;; -----------------------------------------------------------------------------

(require
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(define thread* (box '()))

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
        (define name (lambda () (begin e* ...)))
        (set-box! thread* (cons name (unbox thread*))))]
   [stx
    (raise-user-error 'define-thread (format "Expected (define-thread id expr* ...), got '~a'" (syntax->datum #'stx)))]))

(define-syntax run
  (syntax-parser
   [(_)
    #'(for-each thread-wait (map thread (unbox thread*)))]))
