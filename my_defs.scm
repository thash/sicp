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

; TODO: extend average so that it can be used with any number of arguments
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

; from q1.16
(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))
