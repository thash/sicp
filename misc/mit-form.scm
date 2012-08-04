(load "../my_defs")

(define (mit-form->primitive-form expr)
  (cons (car expr)
        (cons (car (car (cdr expr)))
              (cons (cons 'lambda
                          (cons (cdr (car (cdr expr)))
                                (cdr (cdr expr))))
                    '()))))

(define (primitive-form->mit-form expr)
  (cons (car expr)
        (cons (cons (car (cdr expr))
                    (car (cdr (car (cdr (cdr expr))))))
              (cdr (cdr (car (cdr (cdr expr))))))))

(use gauche.test)
(test-section "convert")

(eqr (mit-form->primitive-form '(define (func a) (print a)))
     => '(define func (lambda (a) (print a))))

(eqr (primitive-form->mit-form '(define func (lambda (a) (* a a))))
     => '(define (func a) (* a a)))


