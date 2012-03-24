(add-load-path ".")
(load "q2.18")

(define x (list (list 1 2) (list 3 4)))
;=> ((1 2) (3 4))

; gosh> (reverse x)
; ((3 4) (1 2))
; gosh> (deep-reverse x)
; ((4 3) (2 1))

(define (deep-reverse items)
  (if (null? items)
    ()
    (if (list? items)
      (append (deep-reverse (cdr items)) (cons (deep-reverse (car items)) ()))
      items
      )))

; appendの行をはじめ
; (append (deep-reverse (cdr items)) (deep-reverse (car items)))
; としていたが、エラー。#?=で見てみると
; !!!*** ERROR: list required, but got 4!!! となっていた。ので、nilとconsして強引にlistにする。
