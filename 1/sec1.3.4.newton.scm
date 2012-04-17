(add-load-path ".")
(load "my_defs")
(load "sec1.3.4")

(define dx 0.00001)
(define (deriv g) ; definition of derivation
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
          dx)))

;(define (cube2 x) (* x x x))
;(print ((deriv cube2) 5))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
;(print (sqrt 4))


(define (fixed-point-of-transform g transform guess) ; transform ... 推測値をずらす手続き
  (fixed-point (transform g) guess))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
