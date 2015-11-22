Little Book of Semaphores
===

Racket support for implementing the [Little Book of Semaphores](http://www.greenteapress.com/semaphores).

(Still under construction)


Install
---

Clone & use `raco`:

```
> git clone https://github.com/bennn/little-book-of-semaphores
> raco pkg install ./little-book-of-semaphores
```

If you rename the directory before calling `raco`, you can use a shorter name.


Usage
---

Start files with:

```
#lang racket
(require little-book-of-semaphores)

```

Then you can do things like:

```
;; Goal:
;; - run (a1 a2) and (b1 b2) in separate threads
;; - a1 happens before b2
;; - b1 happens before a2

(define-event* a1 a2 b1 b2)

(define a1-done? (make-semaphore 0))
(define b1-done? (make-semaphore 0))

(define-thread A
  (a1)
  (signal a1-done?)
  (wait b1-done?)
  (a2))

(define-thread B
  (b1)
  (signal b1-done?)
  (wait a1-done?)
  (b2))

(module+ main (run))
```


For a little more explanation:

- `(define-event A)` or `(define-event* A ...)` creates a thunk named `A`.
  Threads can call the thunk to pretend they've done something.
- `(define-thread id e* ...)` declares a thread named `id` that will perform
  the sequence of actions `e* ...` when run. The library adds random delays and sleeps between actions.
- `(run)` starts all declared threads and waits for them all to finish
- `signal` and `wait` are alternatives to `semaphore-post` and `semaphore-wait`

My solutions are in the `problems/` folder.

