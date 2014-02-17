(load "./sec3.5.4") ;; include sec3.5.3, stream, my_defs
;; 同次二階線形微分方程式
;;     d^2y/dt^2 - a * dy/dt - by = 0
;; にを扱う信号処理プログラムを書く.

; * ddy -(integral)-> dy -(integral)-> y
; * dyとyを使いddyを定義

(define (solve-2nd a b dt y0 dy0)
  (define   y (integral (delay  dy)  y0 dt))
  (define  dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

; gosh> (define s (solve-2nd 1 1 0.01 10 20))
; gosh> (stream-head s 10)
; 10, 10.2, 10.402999999999999, 10.609049999999998, 10.818200799999998, 11.030504012999998, 11.246012078209999, 11.4647782744734, 11.686856733907254,

;; 3.51 遅延評価と一緒に代入を使うとカオスになるよ, という知見

