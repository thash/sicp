(add-load-path ".")
(load "sec2.1.4")

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (car interval))
(define (lower-bound interval) (cdr interval))

; test, (lower-bound (make-interval 0.7 1.3))
