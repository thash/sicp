(add-load-path ".")
(load "my_defs")
(load "sec1.3.4.newton")

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a (* x x)) (* b x) c)))

(newtons-method (cubic 3 2 1) 1)

; try same value with http://www.serendip.ws/archives/476.
; result is same.
(newtons-method (cubic 2 3 4) 1)

