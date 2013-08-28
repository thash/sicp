(add-load-path ".")
(load "my_defs")
(load "./sec2/sec2.2.3.scm")

;; (hoge n s) n以下の異なる正の整数i,j,kのうち、和がsとなる組をすべてみつけるような
;; sum-is-s? を作る.
;; usage
(filter sum-is-s? (list 1 2 3))
; (hoge 5 6) => ((1 2 3) ... )

; filter系でlist以外の引数も取るようなもの作れる？
;  => 一個上の手続きからsを与えてやればいい
; require "accumulate"
(define (sum-is-s? items)
  (= s (accumulate + 0 items)))
;; => *** ERROR: unbound variable: s


; unique-triplesを定義する("相異なる正の整数i,j,k"を実現)
; 最終形。最初の範囲を1..nではなく1..n-2 とするのは前回の模倣
(define (hoge n)
  (map __proc__ (enumerate-interval 1 (- n 2))))

; __proc__ は...
(lambda (i) (map __proc2__ (enumerate-interval i (- n 1))))
; ちがった、こうだ
(lambda (i) (map __proc2__ (enumerate-interval (+ i 1) (- n 1))))


; __proc2__ は...
(lambda (j) (map __proc3__ (enumerate-interval j n)))
; こっちもjと重複しないようにする。
(lambda (j) (map __proc3__ (enumerate-interval (+ j 1) n)))

; __proc3__ は...
(lambda (k) (list i j k))

; flatmapの場所、初期値など調整して完成。
; require "flatmap", "enumerate-interval"
(define (unique-triples n)
  (flatmap
    (lambda (i) (flatmap
                  (lambda (j) (map
                                (lambda (k) (list i j k))
                                (enumerate-interval (+ j 1) n)))
                  (enumerate-interval (+ i 1) (- n 1))))
    (enumerate-interval 1 (- n 2))))
;; gosh> (unique-triples 5)
;; ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))

; 回答。
; require "accumulate", "filter", "unique-triples"
(define (sum-s-lists n s)
  (define (sum-is-s? items)
    (= s (accumulate + 0 items)))
  (filter sum-is-s? (unique-triples n)))

; gosh> (sum-s-lists 5 6)
; ((1 2 3))
; gosh> (sum-s-lists 6 10)
; ((1 3 6) (1 4 5) (2 3 5))

