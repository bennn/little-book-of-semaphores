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

This imports the file `main.rkt` and installs convenient syntax:

- `(define-event A)` or `(define-event* A ...)` creates a thunk named `A`.
  Threads can call the thunk to pretend they've done something.
- `(define-thread id e* ...)` declares a thread named `id` that will perform
  the sequence of actions `e* ...` when run. The library adds random delays and sleeps between actions.
- `(run)` starts all declared threads and waits for them all to finish
- `signal` and `wait` are alternatives to `semaphore-post` and `semaphore-wait`

My solutions are in the `problems/` folder.
See those for examples.

