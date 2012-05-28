(load "./my_defs")
(load "./sec2.5.2")

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

; 1+2i + 5 => 6+2i
(define z (make-complex-from-real-imag 1 2))
(define n 5)

(eqr (add-complex-to-schemenum z n)
     => '(rectangular 6 . 2))

(test-section "scheme-number->complex")
(eqr (scheme-number->complex (make-scheme-number 3))
     => '(complex rectangular 3 . 0))

(test-section "coercion-table")
(put-coercion 'scheme-number 'complex scheme-number->complex)
(eqr (get-coercion 'scheme-number 'complex)
     => scheme-number->complex)
