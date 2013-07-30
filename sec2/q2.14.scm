(add-load-path ".")
(add-load-path "./sec2")
(load "q2.12") ;; make-center-percent を使いたい

;; Lem曰く. 並列抵抗の式は
;;   (1) R1R2/(R1+R2)
;;   (2) 1/(1/R1+1/R2)
;; の2通りの式で表現できるはずなのに, Alyssaのシステムだと両者が等しくならない.

; (1) R1R2/(R1+R2)
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; (A). Lemが正しいことを示せ.

;; (B). 計算結果を調べる.
(define i (make-center-percent 1000 3)) ;; 1kオームの抵抗で誤差3%
(define j (make-center-percent 500 3))

(display (percent (par1 i j))) ;; => 8.978458162960006 (約9%)
(display "\n")
(display (percent (par2 i j))) ;; => 3.0000000000000004 (約3%. もともと与えた3%の誤差がそのまま出てる)

