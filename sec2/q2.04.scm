(add-load-path ".")
(load "my_defs")

; problem
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; answer -- まだここでは置き換えモデルが使える.
; (car (cons x y))
; => (car (lambda (m) (m x y)))
; => ((lambda (m) (m x y)))

(define (cdr z)
  (z (lambda (p q) q)))


