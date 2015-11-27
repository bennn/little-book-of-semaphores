#lang little-book-of-semaphores

(define CAPACITY 2)

(define bouncer (make-semaphore CAPACITY))

(define-syntax-rule (make-clubber name* ...)
  (begin
    (define-thread name*
      (wait bouncer)
      (printf "~a is in the club\n" (object-name name*))
      (random-sleep)
      (signal bouncer)
      (printf "~a says goodbye\n" (object-name name*))) ...))

(make-clubber
  peter jasmine sarah susan greg scarlet erluo roman ben)

(module+ main (run))
