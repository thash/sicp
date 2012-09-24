;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.4. ストリームと遅延評価
(load "./sec3.5.3") ; include stream, my_defs


; (define int
;   (cons-stream initial-value
;                (add-streams (scale-stream integrand dt) int)))


;; 微分方程式を解く

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)


(define (integral delayed-integrand initial-value dt)
  (define int (cons-stream initial-value
                           (let ((integrand (force delayed-integrand)))
                             (add-streams (scale-stream integrand dt)
                                          int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(display-stream-n (solve (lambda (y) y) 1 0.001) 20)
(newline)
(display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))

;; fの形が f(y) = y <=> dy/dt = y. 微分しても形が変わらないといえば底がeの指数関数e^x.
;; 1000分の1刻みで1000個目, つまりt=1を取るとeそのもの(2.71...)が求められるよ, という意図

