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
