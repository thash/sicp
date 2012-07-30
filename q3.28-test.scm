(load "./my_defs")
(load "./q3.28")

(use gauche.test)
(test-section "logical-or")
(eqr (logical-or 1 1) => 1)
(eqr (logical-or 1 0) => 1)
(eqr (logical-or 0 1) => 1)
(eqr (logical-or 0 0) => 0)


(test-section "half-adder")


