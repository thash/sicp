(load "./my_defs")
(load "./q2.80")

;; installation
(use gauche.test)
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(test-start "q2.80")
(test-section "scheme-number - =zero?")
(eqr (=zero? (make-scheme-number 0)) => #t)
(eqr (=zero? (make-scheme-number 1)) => #f)

(test-section "rational - =zero?")
(eqr (=zero? (make-rational 0 1)) => #t)
(eqr (=zero? (make-rational 0 2)) => #t)
(eqr (=zero? (make-rational 1 2)) => #f)

(test-section "complex - =zero?")
(eqr (=zero? (make-complex-from-real-imag 2 0)) => #t)
(eqr (=zero? (make-complex-from-real-imag 2 1)) => #f)
