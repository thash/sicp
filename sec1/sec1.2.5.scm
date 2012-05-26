(define (gcd a b)
  (if (= b 0)
    a
;    #?=(gcd #?=b #?=(remainder a b))))
    (gcd b (remainder a b))))

(gcd 15 302)

