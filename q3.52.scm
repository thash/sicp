(load "./my_defs")

;;; memoあるなし切り替えて出力の違いを見る ;;;
(load "./stream")
;(load "./stream-without-memo")


(define sum 0) ; sum 0, without-memo 0
(define (accum x)
  (set! sum (+ x sum))
  sum)
; sum 0, without-memo 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum 1, without-memo 1
(define y (stream-filter even? seq))
; sum 6, without-memo 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; sum 10, without-memo 15
(stream-ref y 7)
; sum 136, without-memo 162
(display-stream z)
; sum 210, without-memo 362
; => 10
;    15
;    45
;    55
;    105
;    120
;    190
;    210

; without-memo
; => 15
;    180
;    230
;    305

