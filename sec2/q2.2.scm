(add-load-path ".")
;;(load "my_defs")

;; 人間のために座標を座標っぽく出力する関数
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; pointは, xとyの座標からなる.
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; segmentは, ふたつのpointを結ぶことで定義できる.
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

;;; 使ってみる.
;; gosh> (define p1 (make-point 1 2)) ;;=> (1 . 2)
;; gosh> (define p2 (make-point 3 6)) ;;=> (3 . 6)
;; gosh> (define seg1 (make-segment p1 p2)) ;;=> ((1 . 2) 3 . 6)


(define (midpoint p1 p2)
  (make-point
  (average (x-point p1) (x-point p2))
  (average (y-point p1) (y-point p2))
  ))

(define (midpoint-segment segment)
  (midpoint
    (start-segment segment)
    (end-segment segment)))
