(load "./my_defs")
(load "./q2.81")

;; installation
(use gauche.test)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-scheme-number-package)
(install-additional-complex)

;; test start
(test-start "sec2.5.2")
(test-section "add-complex-to-schemenum")
