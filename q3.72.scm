;; 今度は 3通りの異なる方法で2つの平方数の和として掛ける数のストリームを生成せよ。
;; 遅延/無限streamについて、RubyでもEnumerable Lazyとかgem入れればok
(load "./q3.71") ;include q3.70, sec3.5.3


(define (ramanujan3 s)
  (let ((s1car (stream-car s))
        (s2car (stream-car (stream-cdr s)))
        (s3car (stream-car (stream-cdr (stream-cdr s)))))
    (let ((s1-weight (square-weight s1car))
          (s2-weight (square-weight s2car))
          (s3-weight (square-weight s3car)))
      (if (= s1-weight s2-weight s3-weight)
        (begin
          ;; debug print...
          ;(newline)
          ;(display s1-weight)
          ;(display s1car)
          ;(display s2car)
          ;(display s3car)
          (cons-stream s1-weight
                       (ramanujan3 (stream-cdr s))))
        (ramanujan3 (stream-cdr (stream-cdr s)))))))


(define (square-weight pair)
  (+ (square (car pair)) (square (cadr pair))))

(define squares (weighted-pairs square-weight integers integers))

(define s3 (ramanujan3 squares))
(display-stream-n s3 10)
; 650  - (5 25) (11 23)(17 19)
; 725  - (7 26) (10 25)(14 23)
; 1025 - (1 32) (8 31) (20 25)
; 1105 - (4 33) (9 32) (12 31)
; 1105 - (9 32) (12 31)(23 24)
; 1325 - (10 35)(13 34)(22 29)
; 1445 - (1 38) (17 34)(22 31)
; 1625 - (5 40) (16 37)(20 35)
; 1625 - (16 37)(20 35)(28 29)

