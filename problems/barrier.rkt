#lang little-book-of-semaphores

(define barrier%
  (class object%
    (super-new)
    (init-field N) ;; Natural
    (field
      [count N]
      [mutex (make-semaphore 1)]
      [can-enter? (make-semaphore 0)]
      [can-leave? (make-semaphore 0)])

    (define/public (barrier-wait)
      (call-with-semaphore mutex
        (lambda ()
          (set-field! count this (sub1 count))
          (when (zero? count)
            (signal can-enter? N))))
      (wait can-enter?))

    (define/public (barrier-signal)
      (call-with-semaphore mutex
        (lambda ()
          (set-field! count this (add1 count))
          (when (= N count)
            (signal can-leave? N))))
      (wait can-leave?))))

(provide barrier%)

;; -----------------------------------------------------------------------------

(module+ test
  (define barrier (make-object barrier% 3))
  (define-thread A
    (printf "A waiting\n")
    (send barrier barrier-wait)
    (printf "A signalling\n")
    (send barrier barrier-signal))
  (define-thread B
    (printf "B waiting\n")
    (send barrier barrier-wait)
    (printf "B signalling\n")
    (send barrier barrier-signal))
  (define-thread C
    (printf "C waiting\n")
    (send barrier barrier-wait)
    (printf "C signalling\n")
    (send barrier barrier-signal))
  (for ([_i (in-range 5)])
    (run))
)
