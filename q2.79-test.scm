(load "./my_defs")
(load "./q2.79")

;; installation
(use gauche.test)
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package) ;; rectangular/polarがないとこいつだけ使ってもエラる

(test-start "q2.79")
(test-section "scheme-number - equ?")
(eqr (eq? 1 1) => #t)
(eqr (equ? 1 1) => #t)
(eqr (equ? (make-scheme-number 1) (make-scheme-number 1)) => #t)
(eqr (equ? (make-scheme-number 1) (make-scheme-number 3)) => #f)
(eqr (equ? (make-scheme-number 1) 1) => #t)

(test-section "rational - equ?")
(eqr (eq? (make-rational 2 1) (make-rational 2 1)) => #f)
(eqr (equal? (make-rational 2 1) (make-rational 2 1)) => #t)
(eqr (equ? (make-rational 2 1) (make-rational 2 1)) => #t)
(eqr (equ? (make-rational 2 1) (make-rational 2 3)) => #f)

(test-section "complex - equ?")
(eqr (eq? (make-complex-from-real-imag 2 1) (make-complex-from-real-imag 2 1)) => #f)
(eqr (equal? (make-complex-from-real-imag 2 1) (make-complex-from-real-imag 2 1)) => #t)
(eqr (equ? (make-complex-from-real-imag 2 1) (make-complex-from-real-imag 2 1)) => #t)
(eqr (equ? (make-complex-from-mag-ang 10 0.3) (make-complex-from-mag-ang 10 0.3)) => #t)

