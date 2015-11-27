#lang little-book-of-semaphores

(provide
  lightswitch%
  ;; (new lightswitch% [switch (make-semaphore 1)])
  ;; Methods:
  ;; - wait
  ;; - signal

  (rename-out
    [rw-lock% readers-dominate-writers%]
    [Wr-lock% writers-dominate-readers%]
    [wr-lock% readers-wait-for-writer%])
  ;; Variations on the reader-writer lock.
  ;; The names hint at the semantics:
  ;; - readers-dominate : Writers wait for all readers to finish
  ;; - readers-wait     : New readers wait for a queued writer to finish
  ;; - writers-dominate : Readers give priority to writers & wait for all writers to finish
  ;; Methods:
  ;; - reader-enter
  ;; - reader-exit
  ;; - writer-enter
  ;; - writer-exit
)

;; -----------------------------------------------------------------------------

(define lightswitch%
  (class object%
    (super-new)
    (init-field
     switch) ;; Semaphore
    (field
     [members (box 0)]
     [mutex (make-semaphore 1)])

    (define/public (wait)
      (with mutex
        (when (zero? (unbox members))
          (semaphore-wait (get-field switch this)))
        (incr members)))

    (define/public (signal)
      (with mutex
        (decr members)
        (when (zero? (unbox members))
          (semaphore-post (get-field switch this)))))))

;; Readers dominate
(define rw-lock%
  (class object%
    (super-new)
    (field
      [room-empty (make-semaphore 1)]
      [light (new lightswitch% [switch room-empty])])

    (define/public (reader-enter)
      (send (get-field light this) wait))

    (define/public (reader-exit)
      (send (get-field light this) signal))

    (define/public (writer-enter)
      (wait (get-field room-empty this)))

    (define/public (writer-exit)
      (signal (get-field room-empty this)))))

;; Lets waiting writers go first
(define wr-lock%
  (class object%
    (super-new)
    (field
      [room-empty (make-semaphore 1)]
      [switch (new lightswitch% [switch room-empty])]
      [turnstile (make-semaphore 1)])

    (define/public (reader-enter)
      (wait turnstile)
      (signal turnstile)
      (send switch wait))

    (define/public (reader-exit)
      (send switch signal))

    (define/public (writer-enter)
      (wait turnstile)
      (wait room-empty)
      (signal turnstile))

    (define/public (writer-exit)
      (signal room-empty))))

;; Writers dominate
(define Wr-lock%
  (class object%
    (super-new)
    (field
      [no_readers (make-semaphore 1)]
      [no_writers (make-semaphore 1)]
      [mutex (make-semaphore 1)]
      [r_switch (new lightswitch% [switch no_readers])]
      [w_switch (new lightswitch% [switch no_writers])])

    (define/public (reader-enter)
      (wait no_writers)
      (send r_switch wait)
      (signal no_writers))

    (define/public (reader-exit)
      (send r_switch signal))

    (define/public (writer-enter)
      (send w_switch wait)
      (wait mutex))

    (define/public (writer-exit)
      (signal mutex)
      (send w_switch signal))))

;; -----------------------------------------------------------------------------

(module+ test
  (define lock
    ;(new rw-lock%))
    (new wr-lock%))
    ;(new Wr-lock%))

  (define-syntax-rule (make-reader id* ...)
    (begin
      (define-thread id*
        (send lock reader-enter)
        (printf "~a reading\n" (object-name id*))
        (send lock reader-exit)) ...))

  (define-syntax-rule (make-writer id* ...)
    (begin
      (define-thread id*
        (send lock writer-enter)
        (printf "~a writing\n" (object-name id*))
        (send lock writer-exit)) ...))

  (make-reader R1 R2 R3 R4)
  (make-writer W1 W2)
  (make-reader R5 R6 R7)
  (make-writer W3)
  (make-reader R8 R9 R10 R11 R12)

  (run))

