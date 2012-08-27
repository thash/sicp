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
  (cond ((= n 0) 1)
        ((= n 1) x)
    (else (* x (power x (- n 1))))))

;; removed fast-expt => use default "expt"

(define-syntax eqt
  (syntax-rules ()
                ((_ a b)
                 (test* (quote b) a b))))

(define-syntax eqr
  (syntax-rules ()
                ((_ b => a)
                 (test* (quote b) a b))))

(define (puts x)
  (display x)
  (newline))
