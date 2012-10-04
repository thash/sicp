(load "./my_defs")
(load "./q3.3")

(use gauche.test)
(test-section "make-account")

(define acc (make-account 100 'secretdayo))
(eqr ((acc 'secretdayo 'withdraw) 40) => 60)
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error))
(eqr ((acc 'secretdayo 'deposit) 100) => 160)


