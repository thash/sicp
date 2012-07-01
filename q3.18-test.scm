(load "./my_defs")
(load "./q3.18")

(use gauche.test)
(test-start "cdr-looping?")
(define l1 (list 'a))
(define l2 (list 'b 'c))
(define l3 (list 'a 'b 'c))
(set-car! l2 l1)
(set-car! (cdr l2) l1)

(eqr (cdr-looping? l2) => #f)

(define x-inf (list 'a 'b 'c))
(set-cdr! (cdr (cdr x-inf)) x-inf)

(eqr (cdr-looping? x-inf) => #t)

