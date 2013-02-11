;; sec5.4にて "yet-another-ev-sequence" として名前を分けていた一連の手続きを有効にする.

;; 反復版
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;; n=1..10で
;; total-pushes=10, maximum-depth=6の固定になってしまう. どっかにbugがある

;; 再帰版
(define (factorial n)
  (if (= n 1)
    1
    (* (factorial (- n 1)) n)))

;; こちらは (total-pushes = 5 maximum-depth = 3) で固定の値になる


