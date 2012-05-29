(load "./my_defs")
(load "./q2.78")

;; installation
(use gauche.test)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(test-start "q2.78")
(test-section "type-tag")
(eqr (type-tag (make-scheme-number 3)) => 'scheme-number)
(eqr (type-tag 3) => 'scheme-number)

(test-section "contents,")
(eqr (contents (make-scheme-number 3)) => 3)
(eqr (contents 3) => 3)

(test-section "attach-tag")
(eqr (attach-tag 'scheme-number (make-scheme-number 2)) => 2)
(eqr (attach-tag 'scheme-number 3) => 3)
(eqr (attach-tag 'complex '(rectangular 2 . 1)) => '(complex rectangular 2 . 1))

