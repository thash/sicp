; 背景: 測定誤差を組み合わせたとき全体の誤差を知りたい。
; 上端と下端を持つ「区間」というオブジェクトを仮定。
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
; ここでmake-intervalはまだ仮定. q2.7.scm で実装する.

; 限界の積の最大値と最小値を見つけ、それらを結果の区間の限界とすることで、2つの区間の積も作った。
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))
                               )))


