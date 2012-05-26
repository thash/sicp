;; recursive
(define (my-func n)
  (if (< n 3)
    n
    (+ (* 1 (my-func (- n 1)))
       (* 2 (my-func (- n 2)))
       (* 3 (my-func (- n 3))))
    ))

(print (my-func 3))
(print (my-func 4))
(print (my-func 10))

;; interprit..

