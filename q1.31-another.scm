(add-load-path ".")
(load "my_defs")

(define (product term a next b)
  (define (iter a stock)
    (if (> a b)
      stock
      (iter (next a) (* (term a) stock))))
  (iter a 1))


(define (factorial n)
  (define (term a) a)
  (define (next a) (+ a 1))
  (product term 1 next n))

;(print (factorial 5))

; {(2*4)/(3*3)} * {(4*6)/(5*5)} * ....と考える
; => 一般項は (2n*2(n+1))/(2n+1)^2
; from: http://oss.timedia.co.jp/show/SICP/ex-1.31
(define (wallis-pi n)
  (define (term i)
    (/ (* (* 2 i) (* 2 (+ i 1)))
       (square (+ (* 2 i) 1))))
  (define (next i) (+ i 1))
  (* 4 (product term 1 next n)))

(trace product)
(print (exact->inexact (wallis-pi 4)))
(print (exact->inexact (wallis-pi 10)))
(print (exact->inexact (wallis-pi 100)))

