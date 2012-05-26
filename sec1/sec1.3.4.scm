(add-load-path ".")
(load "my_defs")

; from q1.36
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
;  (trace close-enough?)
;  (trace try)
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; fixed-pointは手続きを第一引数に取る。
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))


