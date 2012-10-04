(load "./my_defs")
(load "./q3.4")

(use gauche.test)
(test-section "make-account")

(define acc (make-account 100 'secretdayo))
(eqr ((acc 'secretdayo 'withdraw) 40) => 60)
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 1
(eqr ((acc 'secretdayo 'deposit) 100) => 160)
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 2
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 3
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 4
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 5
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 6
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 7 => 通報しますた
(eqr ((acc 'xxxxxxxxx 'withdraw) 20) => (test-error)) ;; 8 (count reset, 0)



