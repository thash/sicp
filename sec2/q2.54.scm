; = は数値として等しいかどうかを判定し、eq?は記号として正しいかどうかを判定する

(equal? '(this is a list) '(this is a list)) ; => #t
(equal? '(this is a list) '(this (is a) list)) ; => #f

(define (equal2? a b)
  (if (not (pair? a))
    (if (not (pair? b))
      (eq? a b)
      (equal2? a (car b)))
    (if (not (pair? b))
      (equal2? (car a) b)
      (equal2? (car a) (car b)))))

(equal2?  '(this is a list) '(this is a list)) ; => #t
(equal2? '(this is a list) '(this (is a) list)) ; => #t
