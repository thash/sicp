(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))
(define (even? n)
        (= (remainder n 2) 0))

;; use
(define (fast-x-iter a b)
  (x-iter a b 0))

(define (x-iter a b n)
  (cond ((= b 0) n)
        ((even? b) (x-iter (double a) (halve b) n))
        (else (x-iter a (- b 1) (+ n a)))
        )
    )


(print (fast-x-iter 3 4))
(print (fast-x-iter 3 5))
(print (fast-x-iter 1 99))
(print (fast-x-iter 4 99))
