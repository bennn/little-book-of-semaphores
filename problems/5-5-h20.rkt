#lang little-book-of-semaphores

(define bond-id (box 0))
(struct molecule (
  id ;; Natural
  t  ;; (U 'water 'hydrogen)
) #:transparent )

(define water-q (make-semaphore 0))
(define num-water (box 0))
(define hydro-q (make-semaphore 0))
(define num-hydro (box 0))

(define mutex (make-semaphore 1))
(define bond-lock (make-semaphore 1))
(define subbond-begin (make-semaphore 0))
(define subbond-end (make-semaphore 0))

;; -----------------------------------------------------------------------------
;; TODO clean up
;; Generic for water/hydrogen, how to wait
(define (do-wait m this-sema this-ctr other-sema)
  (wait bond-lock)
  (define merge?
    (with mutex
      (incr this-ctr)
      (and (<= 1 (unbox num-water))
           (<= 2 (unbox num-hydro)))))
  (if merge?
    (begin ;; with bond-lock
      (bond m)
      (signal subbond-begin)
      (signal subbond-begin)
      (case (molecule-t m)
       [(water)
        (signal hydro-q)
        (signal hydro-q)]
       [(hydrogen)
        (signal hydro-q)
        (signal water-q)])
      (wait subbond-end)
      (wait subbond-end)
      (incr bond-id)
      (decr num-water)
      (decr num-hydro)
      (decr num-hydro)
      (signal bond-lock))
    (begin
      (signal bond-lock)
      (wait this-sema)
      (wait subbond-begin)
      (signal subbond-end)
      (bond m))))

(define (assemble m)
  (printf "[ASSEMBLE] got ~a\n" m)
  (case (molecule-t m)
   [(water)
    (do-wait m water-q num-water hydro-q)]
   [(hydrogen)
    (do-wait m hydro-q num-hydro water-q)]
   [else
    (raise-user-error
      'assemble
      "Bad molecule type '~a', expected 'wated or 'hydrogen" (molecule-t m))]))

(define (bond m)
  (printf "[BOND:~a] got ~a\n" (unbox bond-id) m)
  (sleep (random 2)))

;; -----------------------------------------------------------------------------

(define-for-syntax m-id (box 0))
(define-syntax (define-molecule stx)
  (syntax-parse stx
   [(_ type:id)
    #:with id-num (unbox m-id)
    #:with id (format-id stx "~a~a" (syntax-e #'type) (syntax-e #'id-num))
    (set-box! m-id (+ 1 (unbox m-id)))
    (syntax/loc stx
      (define-thread id
        (let ([m (molecule 'id-num 'type)])
          (forever (assemble m)))))]))

;; -----------------------------------------------------------------------------

(module+ test
  (define-molecule water)
  (define-molecule water)
  (define-molecule water)
  (define-molecule water)
  (define-molecule hydrogen)
  (define-molecule hydrogen)
  (run))
