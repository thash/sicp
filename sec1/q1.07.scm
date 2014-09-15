;; 本文中で定義したgood-enough?は小さい/大きい数値の平方根を見つけるときに制度がイマイチ.
;; そこで代替戦略
;;     > guessがある試行から次に向けどのように変化するか監視し, 変化が推定値の割合においてとても小さい場合に止めることである
;; つまり0.01という決め打ちthresholdではなく, 現在の推定値から見て小さいかどうかを判定する.

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
        (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 1.0))
(print (my-sqrt 1.0))

(print "-------------------")

;; [XXX] My good-enough? definition.
(define (good-enough2? guess x)
  (define delta (abs (- (square guess) x)))
  (print "good-enough2?")
  (print delta)
  (print (/ delta guess))
  (< (/ delta guess) 0.001 )
  )

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
    guess
    (sqrt-iter2 (improve guess x)
               x)))

(define (my-sqrt2 x)
  (sqrt-iter2 1.0 x))

;;(print (my-sqrt2 1000000000))

(print "-------------------")
(print "(guess), (improve guess x), (/ guess (improve guess x))")
(print "How to improve: return (average guess (/ x guess))")

(define (good-enough3? guess x)
  (print guess)
  (print (improve guess x))
  (print (/ guess (improve guess x)))
  (print "--one-loop--")
  (< (abs (- 1.0 (/ guess (improve guess x)))) 0.001 ))

(define (sqrt-iter3 guess x)
  (if (good-enough3? guess x)
    guess
    (sqrt-iter3 (improve guess x)
               x)))

(define (my-sqrt3 x)
  (sqrt-iter3 1.0 x))

;;(print (my-sqrt3 9))
;;(print (my-sqrt3 9e-99))
(print (my-sqrt3 1e100))


;; 2014 解き直す
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt x) (sqrt-iter 1.0 x))

;; 改良版good-enough
(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) ;; もう一段階先を計算し, guessの変化量を得る
        guess)                            ;; guess変化量の, 元guessに対する割合を見る
     0.001))                              ;; 変化量の割合が目的精度より小さければgood.

;; 元々のgood-enough?で出る値
(sqrt 0.00009) ;; 0.03220324381282134
;; 改良版good-enough?で出る値
(sqrt 0.00009) ;; 0.009487978730289174

;; 正しい値は 0.00948683... なので, 精度が上がっている

;; 大きい方も確かめる.
(define (power x n)
  (if (= n 1) x (* x (power x (- n 1)))))

;; 答え 1.125899906842624 × 10^15
(sqrt (power 2 100)) ;; 元々の 1.125899906842624e15
(sqrt (power 2 100)) ;; 改良版 1.1266555550173558e15

;; あれ, 精度下がってる...
