;; 木構造再帰のFibonacci計算:
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
;; のスタック演算を監視せよ

(load "./sec5.4")
(start eceval)

(fib 1)  ; => (total-pushes =   16 maximum-depth =  8)
(fib 2)  ; => (total-pushes =   72 maximum-depth = 13)
(fib 3)  ; => (total-pushes =  128 maximum-depth = 18)
(fib 4)  ; => (total-pushes =  240 maximum-depth = 23)
(fib 5)  ; => (total-pushes =  408 maximum-depth = 28)
(fib 6)  ; => (total-pushes =  688 maximum-depth = 33)
(fib 7)  ; => (total-pushes = 1136 maximum-depth = 38)
(fib 8)  ; => (total-pushes = 1864 maximum-depth = 43)
(fib 9)  ; => (total-pushes = 3040 maximum-depth = 48)
(fib 10) ; => (total-pushes = 4944 maximum-depth = 53)

;; (a). maximum-depthのn式
;;      maximum-depth = 5 * n + 3

;; (b).  tp(n=1) = 16, tp(n=2) = 72
;;       tp(n:n>2) = tp(n-1) + tp(n-2) + 40
;;       自力で考えてたけどヒント与えられてた. tp(n) = S(n)
;;         S(n) =  a * Fib(n+1) + b
;;       という形に表す. 代入して計算すれば
;;         S(n) = 56 * Fib(n+1) - 40
