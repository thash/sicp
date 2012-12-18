(load "./sec4.2-lazy")

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; "通常の作用的順序のSchemeで" とあるがGaucheは遅延評価できるので正規順序っぽい?
;; Gaucheで評価すると無限ループになる.

;;; (driver-loop)起動して実行するとこうなる
;   ;;; M-Eval input:
;   (factorial 5)
;   !!!Stack Trace:!!!
;   !!!_______________________________________!!!
;   !!!  0  (eval (operator exp) env)!!!
;   !!!        At line 81 of "./sec4.2-lazy.scm"!!!
;   !!!  1  (eval input the-global-environment)!!!
;   !!!        At line 420 of "./sec4.2-lazy.scm"!!!
;   !!!!!!(factorial 5)

;; 解答 http://wqzhang.wordpress.com/2009/11/29/sicp-exercise-4-25/
;; 作用的順序(applicative-order)では動かない.
;; なぜならunless2つめの引数が無限再帰になるため.
;; n=1の時評価を止めてくれればいいんだけど, (= n 1)が成り立っても再帰factorialを延々計算しようとする(作用的順序とはそういうもの). 0, -1, -2..となっても止まらない
;; つーわけで正規順序(normal-order)だと動く.


