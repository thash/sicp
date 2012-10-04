(load "./my_defs")
(load "./q3.1")

(use gauche.test)
(test-section "make-accumulator")
(define A (make-accumulator 10))
(eqr (A 10) => 20)
(eqr (A 10) => 30)

