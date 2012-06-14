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

;; (trace equ?)
;; (trace raise)
;; (trace project)
;; (trace drop)
;; (trace droppable?)

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

(test-section "[without tags] project scheme-number->ratioanl")
(eqr (project 2) => '(rational 2 . 1))
(eqr (project 10/3) => '(rational 10 . 3))
(eqr (project (sqrt 3)) => '(rational 3900231685776981 . 2251799813685248))

(test-section "project complex->scheme-number")
 ;; 2.78でscheme-numberはtagなしverを手に入れたのであった
(eqr (project (make-complex-from-real-imag 2 5))    => 2)
(eqr (project (make-complex-from-real-imag 2 0))    => 2)
(eqr (project (make-complex-from-real-imag 0 3))    => 0)
(eqr (project (make-complex-from-real-imag 0.9 3))  => 0.9)
(eqr (project (make-complex-from-real-imag 3/10 8)) => 3/10)


(test-start "q2.85 -- drop")
(eqr (drop (make-rational 2 7)) => '(rational 2 . 7))
(eqr (drop (make-rational 2 4)) => '(rational 1 . 2))
(eqr (drop (make-rational 2 1)) => '(integer . 2))
(eqr (drop '(rational 3 . 1)) => '(integer . 3))

(eqr (drop (make-scheme-number 2.1)) => 2.1)
(eqr (drop (make-scheme-number 1/2)) => '(rational 1 . 2))
(eqr (drop (make-scheme-number 10)) => '(integer . 10))

(eqr (drop (make-complex-from-real-imag 3/10 8)) => '(complex rectangular 3/10 . 8))
(eqr (drop (make-complex-from-real-imag 3/10 0)) => 3/10)
(eqr (drop (make-complex-from-real-imag 10/3 0)) => '(rational 10 . 3))
(eqr (drop (make-complex-from-real-imag 3 0))    => '(integer . 3))


(test-start "q2.85 -- apply-generic")
(eqr (add (make-rational 4 3) (make-rational 5 3)) => '(integer . 3))

;; あれ？ これ通らない... なんでinteger同士計算しようとするのか。
(eqr (add (make-complex-from-real-imag 4 3)
          (make-complex-from-real-imag 5 -3)) => 9)
;; うお、テストコケてた。上の方で。



