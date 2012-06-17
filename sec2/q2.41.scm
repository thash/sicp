(add-load-path ".")
(load "my_defs")

; (hoge n s) n以下の異なる正の整数i,j,kのうち、和がsとなる組をすべてみつける
; (hoge 5 6) => ((1 2 3) ... )

; 候補リスト (enumerate-interval 1 n) を filterする。
; usage
(filter sum-is-s? (list 1 2 3))

; accumulateの定義 from sec2.2.3.scm
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; filter系でlist以外の引数も取るようなもの作れる？
;  => 一個上の手続きからsを与えてやればいい
; require "accumulate"
(define (sum-is-s? items)
  (= s (accumulate + 0 items)))


; unique-triplesを定義する
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

