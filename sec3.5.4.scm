;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.4. ストリームと遅延評価
(load "./sec3.5.3") ; include stream, my_defs


; (define int
;   (cons-stream initial-value
;                (add-streams (scale-stream integrand dt) int)))


;; 微分方程式を解く solve 手続きを作ってみるが,
;; yとdyの定義がお互いを参照しているため, 動かない.
;
;    (define (solve f y0 dt)
;      (define y (integral dy y0 dt))
;      (define dy (stream-map f y))
;      y)
;
;; ところが, 原理的には引数の一部が分かっていれば評価し始めることはできる.
;; 遅延評価の出番. 次のようにintegralを再定義する.

;; delayed-integrandに注目.
(define (integral delayed-integrand initial-value dt)
  (define int (cons-stream initial-value
                           (let ((integrand (force delayed-integrand)))
                             (add-streams (scale-stream integrand dt)
                                          int))))
  int)

;; こうして再実装したintegralを使えば,
;; integralに渡すdyにdelayをかけdelayed-objectとして扱うことで solveを動かすことが出来る.

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)



; (display-stream-n (solve (lambda (y) y) 1 0.001) 20)
; (newline)
; (display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))

;; fの形が f(y) = y <=> dy/dt = y. 微分しても形が変わらないといえば底がeの指数関数e^x.
;; 1000分の1刻みで1000個目, つまりt=1を取るとeそのもの(2.71...)が求められるよ, という意図

