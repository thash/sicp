(add-load-path ".")
(load "my_defs")

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+
                   (f (- x dx))
                   (f x)
                   (f (+ x dx))) 3)))

(define (n-fold-smoothed f n)
  (repeated smooth n)
  )

; test from http://wiki.drewhess.com/wiki/SICP_exercise_1.44
(define (impulse-maker a y)
  (lambda (x)
    (if (= x a) y 0)))

(define my-impulse-function
  (impulse-maker 0 6))

(my-impulse-function -1)
(my-impulse-function -0.5)
(my-impulse-function 0) ; => 6
(my-impulse-function 1)
(my-impulse-function 2)
(my-impulse-function 6)

((smooth my-impulse-function) 0) ; => 2 ... (0+6+0)/3
((smooth (smooth (smooth my-impulse-function))) 0) ; => 14/9

((n-fold-smoothed my-impulse-function 1) 0)


