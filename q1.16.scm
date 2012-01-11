;; from http://www.serendip.ws/archives/373

(define (fast-expt-iter b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (* b b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* b a))))
  )

(print (fast-expt-iter 2 5))
