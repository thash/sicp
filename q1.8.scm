(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))


;;  (print guess)
;;  (print (improve guess x))
;;  (print (/ guess (improve guess x)))
;;  (print "--one-loop--")
(define (cube-good-enough? guess x)
  (< (abs (- 1.0 (/ guess (cube-improve guess x)))) 0.001 ))

(define (cube-iter guess x)
  (if (cube-good-enough? guess x)
  guess
  (cube-iter (cube-improve guess x)
             x)))

(define (my-curt x)
  (cube-iter 1.0 x))

(print (my-curt 8))
(print (my-curt 27))
