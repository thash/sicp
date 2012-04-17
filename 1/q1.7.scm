(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
        (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 1.0))
(print (my-sqrt 1.0))

(print "-------------------")

;; [XXX] My good-enough? definition.
(define (good-enough2? guess x)
  (define delta (abs (- (square guess) x)))
  (print "good-enough2?")
  (print delta)
  (print (/ delta guess))
  (< (/ delta guess) 0.001 )
  )

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
    guess
    (sqrt-iter2 (improve guess x)
               x)))

(define (my-sqrt2 x)
  (sqrt-iter2 1.0 x))

;;(print (my-sqrt2 1000000000))

(print "-------------------")
(print "(guess), (improve guess x), (/ guess (improve guess x))")
(print "How to improve: return (average guess (/ x guess))")

(define (good-enough3? guess x)
  (print guess)
  (print (improve guess x))
  (print (/ guess (improve guess x)))
  (print "--one-loop--")
  (< (abs (- 1.0 (/ guess (improve guess x)))) 0.001 ))

(define (sqrt-iter3 guess x)
  (if (good-enough3? guess x)
    guess
    (sqrt-iter3 (improve guess x)
               x)))

(define (my-sqrt3 x)
  (sqrt-iter3 1.0 x))

;;(print (my-sqrt3 9))
;;(print (my-sqrt3 9e-99))
(print (my-sqrt3 1e100))
