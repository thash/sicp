;; 2,3,5以外の因子を持たない整数を数え上げるという問題。以下提示された方針
;;
;; * Sは1から始まる
;; * (scale-stream S 2) の要素はSの要素
;; * (scale-stream S 3) と (scale-stream S 5) についても同様
;; * Sの要素、以上

;; merge ... "2つの順序づけられたストリームを、繰り返しなしに混ぜ合わせて1つの順序づけられたストリームにする手続き"
;; => stream-common.scmに定義した。
(load "./stream")

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S
  (cons-stream 1 (merge
                   (scale-stream S 2)
                   (merge
                     (scale-stream S 3)
                     (scale-stream S 5)))))

; gosh> (display-stream-n S 20)
; 1
; 2
; 3
; 4
; 5
; 6
; 8
; 9
; 10
; 12
; 15
; 16
; 18
; 20
; 24
; 25
; 27
; 30
; 32done
