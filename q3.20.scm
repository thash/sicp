;; sec3.3.1.scmの定義を使って以下の式の評価を示す環境の図を書け。
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(car x) ;=> 17

;; 本当はcondとかでも環境はできるんだけど, 細かいところをはしょってる。

