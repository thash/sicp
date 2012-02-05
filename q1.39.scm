(add-load-path ".")
(load "my_defs")

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (iter (+ i 1))))
      ))
  ;(trace iter)
  (iter 1)
  )


; use iterative cont-frac
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
            (/ (n i) (+ (d i) result)))))
  (trace iter)
  (iter k 0)
  )

; x is radian,
; -2PI <= rad <= 2PI
(define (tan-cf x k)
  (cont-frac
    (lambda (x) (if (= x 1) x (- (* x x)))) ; ここ。マイナス符号付けて返す
    (lambda (x) (- (* 2 x) 1)) ; d
    k
    ))


(print (exact->inexact (tan-cf 1.0 100)))
