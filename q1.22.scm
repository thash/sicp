(add-load-path ".")
(load "my_defs")

;; copy from sec1.2.6.scm

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
        ))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; end copy

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


;; usage
;; (timed-prime-test 199)

;; in question

(define (search-for-primes st ed)
  (search-for-primes-iter st ed))


(define  (search-for-primes-iter st ed)
  (if (<= st ed)
    (and (if (prime? st) ;; and の階層構造が解せぬ。
           (timed-prime-test st))
         (search-for-primes-iter (+ st 1) ed))))

;(search-for-primes 1000 1100)
(newline)
(display "---------")
;(search-for-primes 10000 10100)
(newline)
(display "---------")
;(search-for-primes 100000 100100)
(newline)
(display "---------")
(search-for-primes 1000000 1000100)

