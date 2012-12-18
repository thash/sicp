;; Evaluatorワロタ
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;; を評価した時はどうあるべきか論.
;;
;;     Ben: defineに逐次規則を適用して結果を得るべき
;;     Alyssa: q4.16.scm の立場. エラーになる. (letで渡されてね?)
;;     Eva: aとbの評価が同時ならbを決めるときa=5を利用すべき, 故に20であると.

;; Ben: b = a(=1) + x(=10) = 11, a = 5, then 11 + 5 = 16.

;; 脚注: MIT Scheme likes Alyssa.
;;       原理的にはEvaは正しいが, 常に同時定義を実現する方法が難しい. Benに寄るくらいならAlyssaのようにエラー出すほうがマシ.

;; ちなみにGaucheは20, Evaの立場だ.
; gosh> (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))
; 20

;; Q. Evaの主張を実装するにはどういう方法が考えられるか?
;;    => 例えば, 同時定義されている箇所の変数依存関係を解析して並べ替えて実行してやるとか.


