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

;; (test-start "q2.85")
;; (test-section "drop ratioanl->integer")
;; (eqr (drop (make-rational 2 1)) => '(integer . 2))
;; (eqr (drop (make-rational 2 3)) => '(rational 2 . 3))
;; 
;; (test-section "drop scheme-number->ratioanl")
;; (eqr (drop (make-scheme-number 3)) => '(rational 3 . 1))
;; (eqr (drop (make-scheme-number (sqrt 2))) => '(scheme-number . 1.4142135623730951))
;; ;; 通らない...
;; (eqr (drop (make-scheme-number 0.3)) => '(rational 3 . 10))
;; 
;; (test-section "drop complex->scheme-number")
;; (eqr (drop (make-complex-from-real-imag 1.5 0)) => '(scheme-number . 1.5))
;; (eqr (drop (make-complex-from-real-imag 1.5 1)) => '(complex rectangular 1.5 . 1))

(test-start "q2.85 -- project")
(test-section "project ratioanl->integer")
(eqr (project (make-rational 2 1)) => '(integer . 2))
(eqr (project (make-rational 3 2)) => '(integer . 2))
(eqr (project (make-rational 10 3)) => '(integer . 3))

(test-section "project scheme-number->ratioanl")
(eqr (project (make-scheme-number 2)) => '(rational 2 . 1))
(eqr (project (make-scheme-number 10/3)) => '(rational 10 . 3))
;; 実数(の無理数)->有理数
(eqr (project (make-scheme-number (sqrt 3))) => '(rational 3900231685776981 . 2251799813685248))

(test-section "project complex->scheme-number")
 ;; 2.78でscheme-numberはtagなしverを手に入れたのであった
(eqr (project (make-complex-from-real-imag 2 5))    => 2)
(eqr (project (make-complex-from-real-imag 2 0))    => 2)
(eqr (project (make-complex-from-real-imag 0 3))    => 0)
(eqr (project (make-complex-from-real-imag 0.9 3))  => 0.9)
(eqr (project (make-complex-from-real-imag 3/10 8)) => 3/10)


(test-start "q2.85 -- drop")
