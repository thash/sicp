(add-load-path ".")
(load "my_defs")

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (timed-prime-test2 n)
  (newline)
  (display n)
  (start-prime-test2 n (runtime)))

(define (start-prime-test2 n start-time)
  (if (prime2? n)
    (report-prime2 (- (runtime) start-time))))

(define (prime2? n)
  (= n (smallest-divisor2 n)))

(define (report-prime2 elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next test-divisor)))
        ))


(timed-prime-test2 1009) ; => 13 micro sec (original: 12)
(timed-prime-test2 1013) ; =>  9 micro sec (original: 11)
(timed-prime-test2 1019) ; =>  8 micro sec (original: 11)

(timed-prime-test2 10007) ; => 26 micro sec (original: 34)
(timed-prime-test2 10009) ; => 26 micro sec (original: 50)
(timed-prime-test2 10037) ; => 27 micro sec (original: 34)

(timed-prime-test2 100003) ; => 69 micro sec (original: 106)
(timed-prime-test2 100019) ; => 72 micro sec (original: 105)
(timed-prime-test2 100043) ; => 69 micro sec (original: 105)

(timed-prime-test2 1000003) ; => 252 micro sec (original: 332)
(timed-prime-test2 1000033) ; => 246 micro sec (original: 331)
(timed-prime-test2 1000037) ; => 254 micro sec (original: 330)

; not 2 times faster, but almost 1.5times.
