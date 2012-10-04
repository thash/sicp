;; q3.12.scm の last-pair手続きを使ってmake-cycleを作る。ウロボロス的な。
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; こうなる。へえ
;; gosh> z
;; #0=(a b c . #0#)
;; zを表す箱とポインタ図をかけ。

;; で、(last-pair z)を計算しようとすると...

(last-pair z)

;; 何も返ってこない。CPU爆発したりはしない。
