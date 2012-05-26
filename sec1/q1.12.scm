(add-load-path ".")
(load "my_defs")

(define (pascal n k)
  (cond ((and (= n 1) (= k 1)) 1)
        ((= k 0) 0)
        ((< n k) 0)
        (else (+ (pascal (- n 1) (- k 1))
                 (pascal (- n 1) k)))
  ))

(trace pascal)
(print (pascal 1 1)) ;=> 1
(print (pascal 2 1)) ;=> 1
(print (pascal 3 2)) ;=> 2
(print (pascal 5 3)) ;=> 6


