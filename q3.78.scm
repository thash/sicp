;; 同次二階線形微分方程式
;;     d^2y/dt^2 - a * dy/dt - by = 0
;; を調べる信号処理システムを設計する。


; * ddy -(integral)-> dy -(integral)-> y
; * dyとyを使いddyを定義

(define (solve-2nd a b dt y y0 dy0)
  (define   y (integral (delay  dy)  y0 dt))
  (define  dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)


