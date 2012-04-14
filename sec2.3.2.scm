; 2.3.2. 記号微分
; まず基本的な決まりから実装する。
; dc/dx = 0, dx/dx = 1
; d(u+v)/dx =   du/dx  +   dv/dx
; d(uv)/dx  = u(dv/dx) + v(du/dx)

; 次の選択子, 構成子, 述語は既にあるものとする
; ついでに実装も行う

; (variable? e) 変数かどうか
(define (variable? x) (symbol? x))

; (same-variable? v1 v2) 同じ変数か。
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; (sum? e) eは和か -- 式を形として見ている
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
; (addend e) 加数 -- 和式listの第二項
(define (addend s) (cadr s))
; (augend e) 被加数 -- 和式listの第三項
; 後に(q2.57で)改良
(define (augend s) (caddr s))
; (make-sum a1 a2) -- (+ a1 a2)が印字される
; betterな実装は後述
(define (make-sum-nogood a1 a2)
  (list '+ a1 a2))

; (product? e) eは積か
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
; (multiplier e)
(define (multiplier p) (cadr p))
; (multiplicand e)
; 後に(q2.57で)改良
(define (multiplicand p) (caddr p))
;(make-product m1 m2)
;betterな実装は後述
(define (make-product-nogood m1 m2) (list '* m1 m2))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else)))
; エラー式がエラるので外す
; (error "unknown expression type -- DERIV" exp)

; (deriv '(* (* x y) (+ x 3)) 'x)
; => (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

; 表示の問題を考える。
; 式を簡略化する機能を内包したmake-sum, make-productを作る。

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; andは前の項から評価されて行き、途中で#fが返るとそれ以降は評価されない。
; (= 'x 1) とやるとsyntax errorが出るが、=number?ではそんなことはない。第一項の評価で終わっている。

; require "=number?"
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; ここまでやるとderivがそこそこ使える様になる
; gosh> (deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))
