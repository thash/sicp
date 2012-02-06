(add-load-path ".")
(load "my_defs")

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

; x = log(1000) / log(x)

; (print (fixed-point cos 1.0))
(fixed-point (lambda (x) (/ (log 1000) (log x)))
              2.0) ; => 34 steps

(display "------------------")
(newline)

; with average damping
; (fixed-point (lambda (x) (average (log x) (/ (log 1000) (log x))))
;               2.0) ; => 21 steps
;
; above procedure is wrong

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
              2.0) ; => 9 steps



