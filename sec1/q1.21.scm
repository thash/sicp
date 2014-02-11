;; copy from sec1.2.6.scm
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1) #f
      (= n (smallest-divisor n))))

;; end copy

(print (prime? 1)) ;; => false, になるように強引に追加した
(print (prime? 199)) ;; => true, so 199
(print (prime? 1999)) ;; => true, so 1999
(print (prime? 19999)) ;; => false

(print (smallest-divisor 199))
(print (smallest-divisor 1999))
(print (smallest-divisor 19999)) ;; => 7
