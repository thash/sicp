(add-load-path ".")
(load "sec2.3.2")

(define (power x n)
  (if (= n 1) x
    (* x (power x (- n 1)))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)     (cadr  x))
(define (exponent x) (caddr x))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (power base exponent))
        (else (list '** base exponent))))


; exponentiationの処理を追加
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


