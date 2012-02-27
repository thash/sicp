; Church数

; prob
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; answer
(define one (lambda (f) (lambda (x) (f x)))
(define two (lambda (f) (lambda (x) (f (f x))))

(define inc (lambda (x) (+ x 1))) ;?

; q1.41 で
; (((double (double double)) inc) 5)
; というのをやっている。



