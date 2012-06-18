(load "./my_defs")
(load "./q3.2")

(use gauche.test)
(test-section "make-monitored")
(define s (make-monitored sqrt))
(eqr (s 'how-many-calls?) => 0)
(eqr (s 100) => 10)
(eqr (s 'how-many-calls?) => 1)
(eqr (s 4) => 2)
(eqr (s 'how-many-calls?) => 2)

(eqr (s 'reset-count) => 0)
(eqr (s 'how-many-calls?) => 0)
(eqr (s 9) => 3)
(eqr (s 'how-many-calls?) => 1)

