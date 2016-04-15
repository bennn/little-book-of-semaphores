#lang little-book-of-semaphores

(define CAP 4)
(define boat (box '()))
(define rowing? (box #f))
(define boat-mutex (make-semaphore 1))
(define set-sail (make-semaphore 0))
(define new-boat (make-semaphore 0))

;; -----------------------------------------------------------------------------
;; --- BEGIN DANGER need boat-mutex to call these

(define (boat-full?)
  (= CAP (length (unbox boat))))

(define (boat-empty?)
  (null? (unbox boat)))

(define (boat-sum s)
  (for/sum ([b (in-list (unbox boat))] #:when (eq? b s))
    1))

(define (boat-enter s)
  (set-box! boat (cons s (unbox boat))))

(define (boat-exit s)
  ;; Ignoring s
  (set-box! boat (cdr (unbox boat))))

(define (can-board? s)
  (and (< (length (unbox boat)) CAP)
       (not (unbox rowing?))
       (< (boat-sum s) (- CAP 1))))

;; --- END DANGER
;; -----------------------------------------------------------------------------

(define (row-boat id)
  (printf "~a is rowing the boat: ~a\n" id (unbox boat))
  (sleep 1))

(define-syntax (define-programmer stx)
  (syntax-parse stx
   [(_ ty:id N:nat)
    #:with (name* ...) (for/list ([i (in-range (syntax-e #'N))])
                         (format-id stx "~a~a" (syntax-e #'ty) i))
    (quasisyntax/loc stx
      (begin (define-thread name*
               (forever
                 (wait boat-mutex)
                 (if (can-board? 'ty)
                  (begin
                   (printf "~a boarding\n" 'name*)
                   ;; -- get on the boat
                   (boat-enter 'ty)
                   (if (boat-full?)
                     (begin
                       (set-box! rowing? #t)
                       (row-boat 'name*)
                       (signal set-sail))
                     (begin
                       (signal boat-mutex)
                       (wait set-sail)
                       (wait boat-mutex)))
                   ;; -- get off the boat
                   (printf "~a exiting\n" 'name*)
                   (boat-exit 'ty)
                   (if (boat-empty?)
                     (set-box! rowing? #f)
                     (signal set-sail))
                   (signal new-boat)
                   (signal boat-mutex))
                  (begin
                   (signal boat-mutex)
                   (sleep 1))))) ...))]))

(module+ test
  (define-programmer hacker 20)
  (define-programmer serf   20)
  (run)
)
