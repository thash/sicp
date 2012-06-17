(add-load-path ".")
(load "sec2.1.4")

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; (print
; (lower-bound (make-interval 0.7 1.3))
; (upper-bound (make-interval 0.7 1.3))
; )
