(add-load-path ".")
(load "q2.56")

; augend と multiplicand を変更する必要がある。
; 任意個の引数を取る手続きはq2.20でやった。Rubyで言う*args, Schemeでは . z で表現していた。

; augendは2個目以降の引数の"和"を返して欲しい。ここの"和"にはmake-sumを使う。
; 何回cdrすればnullになるかで項の数が判定できるので、2項の和ならいままで通り、それより多ければ以降をcddrで取りつつ+記号をくっつける。
(define (augend s)
  (if (null? (cdddr s)) (caddr s)
    (cons '+ (cddr s))))
; listでくっつけると(+ (3 4))となるのでcons
; 同じようにmultiplicandを実装。operationを引数にして共通部分を抜き出すことも出来る。
(define (multiplicand s)
  (if (null? (cdddr s)) (caddr s)
    (cons '* (cddr s))))

; 構成手続きが変わったので、ここでq2.56と同じderivを定義しなおし。
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
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        (else)))

; gosh> (deriv '(* x y (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))
