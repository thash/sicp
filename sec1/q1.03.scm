;; 3つの数値を引数として取り, うち2つの大きな数値の二条を返す手続きを定義せよ.
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+  (square x) (square y)))

(define (sum-of-bigger-2-squares x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= y x) (<= y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(sum-of-bigger-2-squares 1 2 3)   ; => 13
(sum-of-bigger-2-squares 3 1 2)   ; => 13
(sum-of-bigger-2-squares 10 20 1) ; => 500
