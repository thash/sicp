(load "./my_defs")
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

(test-section "apply-generic (same type)")
(eqr (apply-generic 'add (make-integer 1) (make-integer 2)) => '(integer . 3))
(eqr (apply-generic 'add (make-rational 2 3) (make-rational 1 1)) => '(rational 5 . 3))

(test-section "apply-generic (different types)")
(eqr (apply-generic 'add (make-rational 2 3) (make-integer 1)) => '(rational 5 . 3))
(eqr (apply-generic 'add (make-scheme-number 3) (make-integer 1)) => '(scheme-number 4))
(eqr (apply-generic 'add (make-rational 1 3) (make-complex-from-real-imag 3 2)) => '(complex rectangular 10/3 . 2))

(test-section "arithmetic operations")
(eqr (add (make-rational 2 3) (make-integer 1)) => '(rational 5 . 3))
(eqr (sub (make-rational 2 3) (make-integer 1)) => '(rational -1 . 3))
(eqr (mul (make-rational 2 3) (make-integer 2)) => '(rational 4 . 3))
(eqr (div (make-rational 2 3) (make-integer 2)) => '(rational 1 . 3))

(use math.const)
(eqr (add (make-rational 2 3) (make-complex-from-real-imag 1 8)) => '(complex rectangular 5/3 . 8))
(eqr (sub (make-rational 2 3) (make-complex-from-real-imag 1 8)) => '(complex rectangular -1/3 . -8))
(eqr (mul (make-integer 4) (make-complex-from-mag-ang 2 (/ pi 3))) => '(complex polar 8 . 1.0471975511965976))
(eqr (div (make-integer 4) (make-complex-from-mag-ang 2 (/ pi 3))) => '(complex polar 2 . -1.0471975511965976))



