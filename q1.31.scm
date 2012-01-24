(add-load-path ".")
(load "my_defs")


; q1.31(a) - まず再帰ver
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))


; productを使ってfactorialを定義せよ
(define (identity x) x)
(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

; (print (factorial 5))
; (print (factorial 7))


; productを使ってpiの近似値を計算

; 2,4,4,6,6,8,8... => (1/2)*(2n+(-1)^n + 3)
; 3,3,5,5,7,7,9... => (1/2)*(2n-(-1)^n + 3)
(define (bunshi n)
  (/ (+ (+ (* n 2) (power -1 n)) 3) 2))
(define (bunbo n)
  (/ (+ (- (* n 2) (power -1 n)) 3) 2))
(define (wallis-term n)
  (/ (bunshi n) (bunbo n)))

(define (wallis n)
  (product wallis-term 1 inc n))

;(trace product)
;(print (exact->inexact (* 4 (wallis 10))))
;(print (exact->inexact (* 4 (wallis 100))))
;(print (exact->inexact (* 4 (wallis 1000))))
; 2,3000になってくると計算時間がかかる。反復の出番。
; nextにincを使って1ずつ増やすのではなくbunshi/bunboあたりを使うべきな気がしてきた


; q1.31(b)
; 反復的プロセスver

;  -----------没--------------------
; (define (prod-iter stock term a next count)
;   (if (= count 0)
;     (exact->inexact stock)
;     (prod-iter (* stock (term a)) term (next a) next (- count 1))))
;
; (define (product2 term a next count)
;   (prod-iter 1 term a next count))
;
; (define (wallis2 n)
;   (product2 wallis-term 1 inc n))
;
; (trace prod-iter)
; (print (* 4 (wallis2 100)))
;
; (define (factorial2 n)
;   (product2 identity 1 inc n))
; (print (factorial2 5))
;  -----------没-------------------- orz


; answer from http://www.serendip.ws/archives/404 ---------->>

; (define (product2 term a next b)
;   (define (iter a stock)
;     (if (> a b)
;       stock
;       (iter (next a) (* (term a) stock))))
;   (iter a 1))
;
; (define (wallis2 n)
;   (product2 wallis-term 1 inc n))
;
; (trace wallis-term)
; (print (exact->inexact (* 4 (wallis2 50))))
;
; <<-----------------------------------------------


(define (product-my term a next b)
  (prod-iter term a next b 1))

(define (prod-iter term a next b stock)
  (if (> a b)
    (exact->inexact stock)
    (prod-iter term (next a) next b (* stock (term a)))
    ))

(define (wallis-my n)
  (product-my wallis-term 1 inc n))

(trace prod-iter)
;(print (* 4 (wallis-my 10)))
(print (* 4 (wallis-my 100)))


