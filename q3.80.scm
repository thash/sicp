(load "./sec3.5.4") ;; integral with delayed-integrand

;; 直列RLC回路
;;
;;   R : 抵抗値
;;   L : 誘導係数
;;   C : 容量
;;
;; 3つの素子の電圧(v), 電流(i)の関係式
;;
;;   vR = iR * R
;;   vL =  L * d/dt(iL)
;;   iC =  C * d/dt(vC)
;;
;; 回路を接続したときの関係式
;;
;;   vR = iL = -iC
;;   vC = vL + vR
;;
;; 以上を組み合わせて微分方程式を作る
;;
;;   d/dt(vC) = -iL/C
;;   d/dt(iL) = (1/L) * vC - (R/L) * iL

;; RLCは「引数としてvC0, iL0をとり(vC . iL) を返すような手続き」を返す.
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dv) vC0 dt))
    (define iL (integral (delay di) iL0 dt))
    (define dv (scale-stream iL (/ -1 C)))
    (define di (add-streams (scale-stream iL (- (/ R L)))
                            (scale-stream vC (/ 1 L))))
    (stream-map (lambda (v i) (cons v i)) vC iL)))


(define RLC1 (RLC 1 1 0.2 0.1))
(define s (RLC1 10 0))

; (display-stream-n s 10)
;; (10 . 0)
;; (10.0 . 1.0)
;; (9.5 . 1.9)
;; (8.55 . 2.66)
;; (7.220000000000001 . 3.249)
;; (5.5955 . 3.6461)
;; (3.77245 . 3.84104)
;; (1.8519299999999999 . 3.834181)
;; (-0.0651605000000004 . 3.6359559)


