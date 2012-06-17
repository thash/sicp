(define x (cons 1 2 ))
; x => (1 . 2)
(car x) ;=> 1
(cdr x) ;=> 2
(define  (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define one-half (make-rat 1 2))

(define (print-rat x)
(newline)
(display (numer x))
(display "/")
(display (denom x)))

;(print-rat one-half)

; better makr-rat.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
