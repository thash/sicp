(use slib)
(require 'trace)

;; return micro seconds.
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (square x) (* x x))
(define (cube x) (* x x x))

