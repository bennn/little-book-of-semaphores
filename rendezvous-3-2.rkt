#lang racket/base

(require "main.rkt")

;; Section 3.2, rendezvous

;; 4 events
(define-event* a1 a2 b1 b2)

;; Goal:
;; - a1 happens before b2
;; - b1 happens before a2

(define a1-done? (make-semaphore 0))
(define b1-done? (make-semaphore 0))

(define-thread A
  (a1)
  (semaphore-post a1-done?)
  (semaphore-wait b1-done?)
  (a2))

(define-thread B
  (b1)
  (semaphore-post b1-done?)
  (semaphore-wait a1-done?)
  (b2))

(module+ main
  (run))
