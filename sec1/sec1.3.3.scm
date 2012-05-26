(add-load-path ".")
(load "my_defs")

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

; searchは引数に間違った符号が与えられたときエラーになるので厄介。
; 符号判定でラップしてやる。
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

; 2と4の間に存在するsin(x) = 0の解を探す。
; (print (half-interval-method sin 2.0 4.0))

; x^3 - 2x - 3 = 0の解を探す。
; (print (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;                             1.0
;                             2.0))


; 関数fの不動点(fixed point)とは、f(x)=xを満たすx.
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
;  (trace close-enough?)
;  (trace try)
  (try first-guess))

;(print (fixed-point cos 1.0))

;(print
;  (fixed-point (lambda (y) (+ (sin y) (cos y)))
;               1.0)
;)

; 平方根の計算も、xに対してy^2 = xとなるyを探すことと読み替えれば、
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
; と書くことが出来るのではないか。 => 残念ながら収束せず、使えない。答えの周りで振動する。
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
; とすれば次の予測値がループしない。

