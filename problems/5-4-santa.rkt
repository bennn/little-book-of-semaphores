#lang little-book-of-semaphores

;; Messy, but seems alright Like a real workshop?

(require racket/set)

(define NUM-REINDEER 9)
(define-for-syntax NUM-REINDEER 9)
(define hitch (make-semaphore 0))
(define deer-mutex (make-semaphore 1))
(define reindeer* (mutable-seteqv))
(define reindeer-back? #f)
(define sleigh (box 0))
(define sleigh-ready (make-semaphore 0))

(define NUM-ELVES 20)
(define-for-syntax NUM-ELVES 20)
(define ELF-THRESHOLD 3) ;; Need 3 elves to flip the "need help" semaphore
(define elves-need-help? #f)
(define elf-mutex (make-semaphore 1))
(define working-elves (make-vector NUM-ELVES #t))
(define waiting-elves (mutable-seteqv))
(define elves-at-door #f) ;; (U #f (List Elf Elf Elf))

(define event (make-semaphore 0))

;; -----------------------------------------------------------------------------
;; Reindeer have priority

(define (deer-ready d-id)
  (printf "Reindeer ~a is back from vacation\n" d-id)
  (with deer-mutex
    (set-add! reindeer* d-id)
    (when (= NUM-REINDEER (set-count reindeer*))
      (set! reindeer-back? #t)
      (signal event))))

(define-syntax (make-deer stx)
  (syntax-parse stx
   [_
    (define define-deer*
      (for/list ([i (in-range NUM-REINDEER)])
        (define d-id (format-id stx "deer-~a" i))
        (define wait-until (random 10 20))
        #`(define-thread #,d-id
            (sleep #,wait-until)
            (deer-ready #,i)
            (wait hitch)
            (get-hitched #,i))))
    #`(begin #,@define-deer*)]))

(define (prepare-sleigh)
  (printf "Santa preparing sleigh\n")
  (with deer-mutex
    (for ([i (in-range NUM-REINDEER)])
      (signal hitch))))

(define (get-hitched r-id)
  (with deer-mutex
    (printf "Reindeer '~a' is hitched\n" r-id)
    (incr sleigh)
    (when (= 9 (unbox sleigh))
      (signal sleigh-ready))))

;; =============================================================================

(define (pop n x*)
  (define r*
    (for/list ([x (in-set x*)] [_i (in-range n)])
      x))
  (for ([x (in-list r*)])
    (set-remove! x* x))
  r*)

(define (get-help elf-id)
  (printf "Elf ~a needs help\n" elf-id)
  (with elf-mutex
    (vector-set! working-elves elf-id #f)
    (set-add!    waiting-elves elf-id)
    (enqueue-elves)))

;; Assumes caller has elf-mutex
(define (enqueue-elves)
  (when (and (<= ELF-THRESHOLD (set-count waiting-elves))
             (not elves-at-door))
    (set! elves-at-door (pop ELF-THRESHOLD waiting-elves))
    (set! elves-need-help? #t)
    (signal event)))

(define-syntax (make-elves stx)
  (syntax-parse stx
   [_
    (define define-elf*
      (for/list ([i (in-range NUM-ELVES)])
        (define e-id (format-id stx "elf-~a" i))
        #`(define-thread #,e-id
            (forever
              (if (vector-ref working-elves #,i)
                (if (zero? (random 8))
                  (begin
                    (printf "Elf ~a is working\n" #,i)
                    (sleep 0.5))
                  (get-help #,i))
                (sleep 0.5))))))
    #`(begin #,@define-elf*)]))

;; =============================================================================

(define-thread santa
  (forever
    (printf "Santa is sleeping\n")
    (wait event)
    (printf "Santa is awake\n")
    (when reindeer-back?
      (prepare-sleigh)
      (wait sleigh-ready)
      (printf "Sleigh is ready for Christmas\n")
      (set! reindeer-back? #f))
    (when elves-need-help?
      (with elf-mutex
        (unless elves-at-door
          (error 'santa "Invariant: elves need help, but no elves at the door"))
        (for ([e (in-list elves-at-door)])
          (sleep 0.5)
          (vector-set! working-elves e #t))
        (printf "Santa helped elves ~a\n" elves-at-door)
        (set! elves-at-door #f)
        (set! elves-need-help? #f)
        (enqueue-elves)))))

;; =============================================================================

(module+ main
  (make-deer)
  (make-elves)
  (run))

