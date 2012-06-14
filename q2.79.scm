;; 二つの数の等価テストをするequ?を定義し、汎用算術演算パッケージに設定せよ。
(load "./sec3.3.3") ;; get, put
(load "./sec2.4.2") ;; attach-tag, contents, etc
(load "./sec2.4.3") ;; install-(rectangular/polar)-package, apply-generic
(load "./sec2.5.1") ;; install-scheme-number/rational-package, complex again
(load "./q2.78")    ;; new attach-tag, contents, type-tag


;; putで追加
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))
(put 'equ? '(rational rational)
     (lambda (x y) (equal? x y)))
(put 'equ? '(complex complex)
     (lambda (x y) (equal? x y)))

;; 汎用手続き化
(define (equ? x y) (apply-generic 'equ? x y))

;; eq?では実数の等しさ判定できない。
;;   gosh> (eq? 1 1) ;;=> #t
;;   gosh> (eq? 10/3 10/3) ;;=> #f
;; ふつーの"="ならできる。実数の等しさチェックはこれにしよう。
;;   gosh> (= 10/3 10/3) ;;=> #t
