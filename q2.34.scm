(add-load-path ".")
(load "q2.33")

; x の多項式のあるxでの値の評価をaccumulateで表せる。
; a_n * x^n + a_(n-1) * x^(n-1) + ... + a_1 * x + a_0
; ↓
; (...(a_n * x + a_(n-1)) * x + ... + a_1) * x + a_0
; 多重のxのかけ算にしてしまう。これをHorner's ruleという。

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) <xxx>)
              0
              coefficient-sequence))
; note: coefficient = 係数

; how to use
(horner-eval 2 (list 1 3 0 5 0 1))
; => 1+3x+5x^3+x^5 にx=2を代入したときの値を返す。正解は79.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

