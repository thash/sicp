(load "./my_defs")
(load "./q2.83")

;; installation
(use gauche.test)
(install-integer-package)
(install-rational-package)
(install-scheme-number-package)

(test-start "q2.83")
(test-section "make-*")
(eqr (make-integer 2)          => '(integer . 2))
(eqr (make-rational 3 2)       => '(rational 3 . 2))
(eqr (make-scheme-number 3.14) => '(scheme-number . 3.14))

(test-section "raise")
(eqr (raise (make-integer 2))          => '(rational 2 . 1))
(eqr (raise (make-rational 3 2))       => '(scheme-number . 3/2))
(eqr (raise (make-scheme-number 3.14)) => '(complex rectangular 3.14 . 0))


