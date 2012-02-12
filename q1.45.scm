(add-load-path ".")
(load "my_defs")
(load "sec1.3.3")
(load "sec1.3.4")
(load "sec1.3.4.newton")
(load "q1.43")

; 関数fの不動点(fixed point)とはf(x) = xを満たす点。
; fixed-point(sec1.3.3), average-damp(sec1.3.4) を使う。
; fixed-point-of-transform(sec1.3.4.newton)

; 収束するかどうかの判断ってどうやるの
; ところでなんでy -> x/y の不動点探索が根になるんだっけ？
; => 関数f(y)においてy=f(y)となる点つまりy=x/y => y^2=x, よってxはyの平方根。法則性はn乗根はy -> x/y^(n-1).
;
; answer from http://wiki.drewhess.com/wiki/SICP_exercise_1.45
(define (fourth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y)))
                            (repeated average-damp 2) ; ここの数字が平均緩和の回数
                            1.0))

(print (fourth-root 16))

(define (eighth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 7)))
                            (repeated average-damp 3) ; ここの数字が平均緩和の回数
                            1.0))

(print (fourth-root (* 3 3 3 3 3 3 3 3)))

; テストを続けて、境目が4と8であることから2^nで区切られているのではないかという仮説を立てている

(define (eighth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 7)))
                            (repeated average-damp 3) ; ここの数字が平均緩和の回数
                            1.0))

(define (fifteenth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 14)))
                            (repeated average-damp 3)
                            1.0))

; bのn乗を計算する手続きfast-exptをmy_defsに追加
; log2(n)
(define (floor-log2 n)
  (define (iter m count)
    (if (= m 0)
        (- count 1)
        ; (iter (arithmetic-shift m -1) (inc count))))
        (iter (ash m -1) (inc count))))
  (iter n 0))

; arithmetic-shift がなかった。Many Scheme implementations implement the arithmetic-shift primitive. とのことだけど。
; 調べてみたらGaucheでは ash = arithmetic-shift なので、これを使う。

; ここまでの知見を元に抽象化。

(define (nth-root x n)
  (let ((number-of-damps (floor-log2 n)))
    (fixed-point-of-transform (lambda (y) (/ x (fast-expt y (- n 1))))
                              (repeated average-damp number-of-damps)
                              1.0)))

; test
(nth-root (fast-expt 2 8) 8)
(nth-root (fast-expt 8 17) 17) ;なんかすげー遠回りな数を経由して時間かかった。



