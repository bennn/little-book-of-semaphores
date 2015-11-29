#lang little-book-of-semaphores

;; One agent, three smokers
;; - smokers need paper+tobacco+matches, then make & smoke
;; - each smoker has one ingredient
;; - agent has all ingredients, makes 2 random ones available at a time

;; -----------------------------------------------------------------------------
;; Agent

(define agent-sem (make-semaphore 1))
(define tobacco (make-semaphore 0))
(define paper (make-semaphore 0))
(define match (make-semaphore 0))

(define-thread Controller
  (forever
    (sleep (random))
    (signal agent-sem)))

(define-thread Agent-A
  (forever
    (wait agent-sem)
    (printf "Make tobacco + paper\n")
    (signal tobacco)
    (signal paper)))

(define-thread Agent-B
  (forever
    (wait agent-sem)
    (printf "Make match + paper\n")
    (signal paper)
    (signal match)))

(define-thread Agent-C
  (forever
    (wait agent-sem)
    (printf "Make match + tobacco\n")
    (signal tobacco)
    (signal match)))

;; -----------------------------------------------------------------------------

(struct num (
  matches
  papers
  tobacco
))

(define mutex (make-semaphore 1))
(define match+paper (make-semaphore 0))
(define match+tobacco (make-semaphore 0))
(define tobacco+paper (make-semaphore 0))

(define pouch (num (box 0) (box 0) (box 0)))

(define-syntax-rule (nonzero? field)
  (not (zero? (unbox (field pouch)))))

(define (try-signal i1 i2 s)
  (when (and (nonzero? i1) (nonzero? i2))
    (decr (i1 pouch))
    (decr (i2 pouch))
    (signal s)))

;; TODO cycle between signal orders

(define-thread Pusher-A
  (forever
    (wait match)
    (with mutex
      (incr (num-matches pouch))
      (try-signal num-matches num-papers match+paper)
      (try-signal num-matches num-tobacco match+tobacco))))

(define-thread Pusher-B
  (forever
    (wait paper)
    (with mutex
      (incr (num-papers pouch))
      (try-signal num-matches num-papers match+paper)
      (try-signal num-papers num-tobacco tobacco+paper))))

(define-thread Pusher-C
  (forever
    (wait tobacco)
    (with mutex
      (incr (num-matches pouch))
      (try-signal num-tobacco num-papers tobacco+paper)
      (try-signal num-matches num-tobacco match+tobacco))))

;; -----------------------------------------------------------------------------

(define (smoke msg)
  (displayln msg)
  (sleep 1))

;; Has matches
(define-thread Smoker-A
  (forever
    (wait tobacco+paper)
    (smoke "Smoker A is lit!")))

;; Has tobacco
(define-thread Smoker-B
  (forever
    (wait match+paper)
    (smoke "Smoker B is lit!")))

;; Has paper
(define-thread Smoker-C
  (forever
    (wait match+tobacco)
    (smoke "Smoker C is lit!")))

;; -----------------------------------------------------------------------------

(module+ test
  (run))
