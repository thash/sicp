(load "./my_defs")
(load "./sec2.5.3") ;; polynomial package. make-polynomial, etc.
(load "./sec3.3.3") ;; put/get
;(load "./q2.78") ;; to make use of normal number as scheme-number. attach-tag, etc.
(load "./q2.85")
(load "./q2.87")

(use gauche.test)
(install-integer-package)
(install-rational-package)
(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

(test-start "q2.87")
(test-section "make-polynomial")
(eqr (make-polynomial 'x '((1 2) (4 5))) => '(polynomial x (1 2) (4 5)))

;; できあがるのは
;;   '(polynomial x ((1 2) (4 5)))
;; だと思ってたけど、'(polynomial x (1 2) (4 5)) だった。

(test-section "=zero?")
(eqr (=zero? (make-polynomial 'x '((1 2) (4 5)))) => #f)
(eqr (=zero? (make-polynomial 'x '((1 0) (4 0)))) => #t)

(eqr (=zero? (add (make-polynomial 'x '((1 1) (4 3)))
                  (make-polynomial 'x '((1 -1) (4 -3))))) => #t)

