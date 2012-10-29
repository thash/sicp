(define (analyze exp)
  (cond ;...
        ((let? exp) (analyze (let->combination exp)))
        ;...
        ))

(define (let->combination exp)
  (if (pair? (car (let-clauses exp)))
    (expand-let-clauses (let-clauses exp))
    (expand-named-let-clauses (let-clauses exp))))

