(add-load-path ".")
(load "my_defs")
(load "sec1.3.3")
(load "sec1.3.4")
(load "sec1.3.4.newton")
(load "q1.43")


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    (lambda (x) (f x))
    (compose f (repeated f (- n 1)))))

(define (n-th-sqrt x n c)
  (fixed-point ((repeated average-damp c) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))


