;; 訳書の問題文がわかりにくい
;; The "width" of an interval is half of the difference between its upper and lower bouds.
;; The width is a measure of ther uncertainty of the number specified by the interval.
;;   "width" は上端と下端の差を2で割ったものである. widthはintervalで表された不確かさの測定値である.
;; For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals,
;; whereas for others the width of the combination is not a function of the widths of the argument intervals.
;;   2つのintervalに行ういくつかの算術演算は, 引数interval2個のwidthの関数で表すことが出来る.
;;   ("いくつか"に含まれない) 他のケースにおいては, そのような関数の形で表すことは出来ない.
;;

(add-load-path ".")
(add-load-path "./sec2")
(load "my_defs")
(load "q2.8")
;; lower-boundとupper-boundを中心点plus/minus幅で表す。

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

(test-section "q2.9")
(define i1 (make-interval 1 3))
(define i2 (make-interval 4 9))
(eqr (width i1) => 1)

;; 幅を足したもの = add-intervalの幅 となることを示す
(eqr (width (add-interval i1 i2))
     =>
     (+ (width i1) (width i2)))

(eqr (width (sub-interval i1 i2))
     =>
     (+ (width i1) (width i2)))

;; 一方でmulとdivはダメ
(display (width i1))
(display ", ")
(display (width i2))
(display "\n")
(display (width (mul-interval i1 i2))) ;; => 23/2
(display "\n")
(display (width (div-interval i1 i2))) ;; => 0.31944444444
(display "\n")
