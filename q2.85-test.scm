(load "./my_defs")
(load "./q2.85")

;; installation
(use gauche.test)
(install-integer-package)
(install-rational-package)
(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-additional-complex)

(test-start "q2.85")
(test-section "drop ratioanl->integer")
(eqr (drop (make-rational 2 1)) => '(integer . 2))
(eqr (drop (make-rational 2 3)) => '(rational 2 . 3))

(test-section "drop scheme-number->ratioanl")
(eqr (drop (make-scheme-number 3)) => '(rational 3 . 1))
(eqr (drop (make-scheme-number (sqrt 2))) => '(scheme-number . 1.4142135623730951))
;; 通らない...
(eqr (drop (make-scheme-number 0.3)) => '(rational 3 . 10))

(test-section "drop complex->scheme-number")
(eqr (drop (make-complex-from-real-imag 1.5 0)) => '(scheme-number . 1.5))
(eqr (drop (make-complex-from-real-imag 1.5 1)) => '(complex rectangular 1.5 . 1))



