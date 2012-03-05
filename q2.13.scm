(add-load-path ".")
(load "q2.7")

; R1-R1P1 + R2-R2P2
; = R1+R2 - (R1P1+R2P2)


; R1+R2 + (R1P1+R2P2) ; これはなに？

; R1R2 / (R1+R2)
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (div-interval one
                (add-interval
                  (div-interval one r1)
                  (div-interval one r2))))

; 二つの結果が異なる。なぜか

