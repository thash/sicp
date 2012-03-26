(cons (list 1 2) (list 3 4))
; => ((1 2) 3 4)

(define x (cons (list 1 2) (list 3 4)))
(length (list x x))


(define (count-leaves x)
  (cond [(null? x) 0]
        [(not (pair? x)) 1]
        [else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))]))

(count-leaves (list x x))

