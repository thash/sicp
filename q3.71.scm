;; Ramanujan数(別名: タクシー数) -- 二通り以上の, 二つの立方数の和で表される数。つまり
;;     n * n * n = X であり、かつ
;;     m * m * m = X である
;; ようなnとmが存在するXを言う。
(load "./my_defs")
(load "./q3.70") ; includes (load "./sec3.5.3")

;(define (find-consective s weight))
;(define ramanujan-pairs
;        (find-consective ij-pairs-371 sum-of-cubes))

(define (cube-weight pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define cubes (weighted-pairs cube-weight integers integers))

; gosh> (display-stream-n cubes 10)
; (1 1), (1 2), (2 2), (1 3), (2 3), (3 3), (1 4), (2 4), (3 4)

;; 「同じ重みで2つ"連続する"対をstreamから探す」
;; cubeの和でsortしている(下準備)ため、連続している2個を調べれば事足りるのがポイント。

(define (ramanujan s)
  (let ((s1car (stream-car s))
        (s2car (stream-car (stream-cdr s))))
    (let ((s1-weight (cube-weight s1car))
          (s2-weight (cube-weight s2car)))
      (if (= s1-weight s2-weight)
        (cons-stream s1-weight
                     (ramanujan (stream-cdr s)))
        (ramanujan (stream-cdr s))))))

;; (= s1-weight s2-weight) がramanujanの条件に合致したことを示す。

(define s (ramanujan cubes))
; (display-stream-n s 10)
; 1729, 4104, 13832, 20683, 32832, 39312, 40033, 46683, 64232,

;; TODO: in Haskell

