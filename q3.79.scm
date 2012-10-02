(load "./sec3.5.4") ;; include sec3.5.3, stream, my_defs
;; 問題3.78のsolve-2ndを一般化する.

(define (solve-2nd f dt y0 dy0)
  (define   y (integral (delay  dy)  y0 dt))
  (define  dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

