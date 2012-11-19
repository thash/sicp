;; 式の失敗を捉えることができる if-fail を実装せよ.

;;;; 使い方 ;;;;
;; if-fail は2つの式を取る. 第一の式を通常に評価し,
;;                            評価が成功すれば通常に戻る.
;;                            評価が失敗すれば, 次の例のように第二の式が戻される:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x
           'all-odd))
;; => 評価すると all-odd が返る.

(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;; => 評価すると 8 が返る.

