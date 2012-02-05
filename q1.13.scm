(add-load-path ".")
(load "my_defs")

; phi, psiを以下の通りとして、Fib(n)が phi^n / sqrt(5) に最も近い整数であることを証明せよ。
; この数は黄金比とも呼ばれる
(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (/ (- 1 (sqrt 5)) 2))
