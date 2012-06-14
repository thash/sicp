(load "./my_defs")
(load "./arithmetic-packages")
(load "./sec2.5.3")

(use gauche.test)
(install-polynomial-package)

(test-start "sec2.5.3")
(test-section "make-polynomial")
(eqr (make-polynomial 'x '((1 1))) => '(polynomial x (1 1)))


