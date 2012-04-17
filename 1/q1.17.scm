(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))
(define (even? n)
        (= (remainder n 2) 0))

(define (* a b)
  (if (= b 0)
    0
    (+ a (* a (- b 1)))))

;; use pencile to understand it
(define (fast-x a b)
  (cond ((= b 1) a)
        ((even? b) #?=(fast-x (double a) (halve b)))
        (else #?=(+ a (fast-x  a (- b 1))))
        )
  )

(print (fast-x 3 4))
(print (fast-x 3 5))
(print (fast-x 1 99))
(print (fast-x 4 99))
