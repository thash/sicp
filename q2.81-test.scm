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
(test-start "q2.81(a,b)")

;; 「scheme-numberパッケージのべき乗の手続きは存在するが他にはない」
;; sec2.5.1.scm を参考に追加定義
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (attach-tag 'scheme-number (expt x y)))) ;; expは指数関数e^x, exptがべき乗演算

(test-section "exp scheme-number")
(define a (make-scheme-number 2))
(define b (make-scheme-number 3))
(eqr (exp a b) => '(scheme-number . 8))

(test-section "exp complex")
(define z1 (make-complex-from-real-imag 2 1))
(define z2 (make-complex-from-real-imag 2 3))
; (eqr (exp z1 z2) => '(scheme-number . 8))
; Error: No method for these types(exp (complex complex))ERROR: GOT #<undef>

(install-q2.81-selfcoercion)
(test-section "Louis: exp scheme-number")
(eqr (exp a b) => '(scheme-number . 8))
(test-section "Louis: exp complex")
; (eqr (exp z1 z2) => '(scheme-number . 8))
;   => 無限ループ

(test-start "q2.81(c)")
; 以降ではapply-genericでapply-generic2を呼び出すようにしたかった {{{1
; (define-syntax apply-generic
;   (syntax-rules ()
;                 ((_ op . args)
;                  (apply-generic2 op . args))))
; (define (apply-generic op . args)
;   (lambda (op args) (apply-generic2 op args))) ; }}}

(trace apply-generic)
(define (exp x y) (apply-generic2 'exp x y))
; gauche で error が使えない
; error発生をexpectしたい
; (eqr (exp z1 z2) => "[Same type] Error: No method for these types")

