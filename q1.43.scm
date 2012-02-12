(add-load-path ".")
(load "my_defs")
(load "q1.42")

; output ... result n

; nの値に関係なく中で1回composeをかまして2回作用させるrepeat-once
(define (repeat-once f n)
  (lambda (x)
    (compose (f (f x)))))

; repeated.
; (define (repeated f n)
;   (lambda (x)
;     (define (iter f n)
;       (if (= n 0)
;         f
;         (compose ((iter f (- n 1)) (f x)))
;         ))
;     (trace iter)
;       (iter f n)
;       ))
;
; ↑中で再帰させようとしていたがrepeated自体を繰り返せばよかった。


; (define (repeated f n)
;   (if (= n 0)
;     f
;     (repeated (compose f f) (- n 1))
;     ))
;
;↑は間違い。1回多くfを適用してしまう。

(define (repeated f n)
  (if (= n 1)
    (lambda (x) (f x))
    (repeated (compose f f) (- n 1))
    ))

; TEST ... OK
; (trace repeated)
; (print ((repeated square 2) 5))
; (print ((repeated square 3) 5))

