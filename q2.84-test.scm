(load "./my_defs")
(load "./q2.83")
(load "./q2.84")

;; installation
(use gauche.test)
(install-integer-package)
(install-rational-package)
(install-scheme-number-package)

(test-start "q2.84")
(test-section "higher")
(eqr (higher 'integer 'rational)       => 'rational)
(eqr (higher 'integer 'complex)        => 'complex)
(eqr (higher 'complex 'integer)        => 'complex)
(eqr (higher 'scheme-number 'rational) => 'scheme-number)
(eqr (higher 'hoge 'integer)           => (test-error))

(test-section "raise-to")
(eqr (raise-to (make-integer 2) 'rational)      => '(rational 2 . 1))
(eqr (raise-to (make-integer 2) 'scheme-number) => '(scheme-number . 2))
(eqr (raise-to (make-integer 2) 'complex)       => '(complex rectangular 2 . 0))
(eqr (raise-to (make-rational 2 3) 'scheme-number) => '(scheme-number . 2/3))
(eqr (raise-to (make-rational 2 3) 'complex)       => '(complex rectangular 2/3 . 0))

(test-section "higher + raise-to")
(eqr (raise-to (make-integer 2) (higher 'rational 'scheme-number)) => '(scheme-number . 2))
(eqr (raise-to (make-integer 2) (higher 'complex 'scheme-number))  => '(complex rectangular 2 . 0))

(test-section "apply-generic")


