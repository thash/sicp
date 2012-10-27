;; Evaluatorワロタ
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;; を評価した時はどうあるべきか論.
;;     Ben: defineに逐次規則を適用して結果を得るべき
;;     Alyssa: q4.16.scm の立場. エラーになる. (letで渡されてね?)
;;     Eva: aとbの評価が同時ならbを決めるときa=5を利用すべき, 故に20であると.

;; 脚注: MIT Scheme likes Alyssa.
;;       原理的にはEvaは正しいが, 常に同時定義を実現する方法が難しい. Benに寄るくらいならAlyssaのようにエラー出すほうがマシ.

;; ちなみにgoshは20, Evaの立場だ.
; gosh> (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))
; 20


