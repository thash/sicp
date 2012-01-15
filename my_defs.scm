(use slib)
(require 'trace)

;; return micro seconds.
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))
(define (cube x) (* x x x))

