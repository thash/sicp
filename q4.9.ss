(define (while->if exp)
  (make-if (cadr exp)
           (sequence->exp (append (cddr exp) (list exp)))
           'ok))


