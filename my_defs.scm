(use slib)
(use math.const)
(require 'trace)

; procedures(?) I've required so far:
; trace, random

(define true #t)
(define false #f)

;; return micro seconds.
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (average x y)
  (/ (+ x y) 2))
(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (power x n) ; xのn乗
  (if (= n 1)
    x
    (* x (power x (- n 1)))))

