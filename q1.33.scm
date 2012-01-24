(add-load-path ".")
(load "my_defs")
(load "sec1.2.5") ;; <= for "gcd" procedure
(load "sec1.2.6.scm") ;; <= for "prime?" procedure

(define (identity x) x)
(define (inc n) (+ n 1))

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))
; 素数じゃない場合は進める。


; q1.33(a) sum of primes between a and b
(define (sum-only-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(trace filtered-accumulate)
; (print (sum-only-prime 1 10))
; (print (sum-only-prime 10 20))

(display "---------------------")
(newline)

; q1.33(b) nと互いに素で、nより小さい正の整数の積
; 互いに素 = nとその数の最大公約数(GCD)が1

(define (product-of-gcds-smaller-than n)
  (define (coprime a) ;互いに素 = coprime
    (= 1 (gcd a n)))
  (filtered-accumulate coprime * 1 identity 1 inc n))

;(print (product-of-gcds-smaller-than 10))
(print (product-of-gcds-smaller-than 20))

; filterとして渡す手続きはaのみを引数にとるので、内部でcoprimeを定義してやる。


