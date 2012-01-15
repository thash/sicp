(add-load-path ".")
(load "my_defs")
(load "sec1.3.1.sum")

; copy from http://oss.timedia.co.jp/show/SICP/ex-1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term i)
    (+ (y (- (* 2 i) 2))
       (* 4 (y (- (* 2 i) 1)))
       (y (* 2 i))))
  (define (next i) (+ i 1))
  (/ (* h (sum term 1 next (/ n 2))) 3))

(print (simpson cube 0 1 100))
