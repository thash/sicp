(add-load-path ".")
(load "my_defs")
(load "sec1.3.3")

; 黄金比phiが(+ 1 (/ 1 x))の不動点であることを示す

; ref q1.13
(define phi (/ (+ 1 (sqrt 5)) 2))

(define (f x) (+ 1 (/ 1 x)))
(print phi)     ;=> 1.618033988749989
(print (f phi)) ;=> 1.618033988749859


; fixed-pointを使ってphiを計算する
(print
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)
)
