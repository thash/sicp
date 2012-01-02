(define (fibonacci n)
  (if (= n 0)
    0
    (if (= n 1)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(print (fibonacci 6))

;; ---------------

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
  b
  (fib-iter (+ a b) a (- count 1))
  ))

;;(define (fib-iter a b counter max-count)
;;  (if (< max-count counter)
;;    a
;;    (fib-iter (+ a b) a (+ counter 1) max-count)
;;   ))

(print (fib 6))
