(load "./sec2/sec2.1.4.scm")

;; sec2.1.4.scm で定義した許容誤差を扱うプログラムの欠けたパーツ,
;; make-intervalを実装する. と言っても構成子/選択子をconsでつくるだけ.
;; かならず (lower . upper) という順で並んでいるという前提.
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; (print
; (lower-bound (make-interval 0.7 1.3))
; (upper-bound (make-interval 0.7 1.3))
; )
