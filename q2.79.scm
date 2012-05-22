;; 二つの数の等価テストをするequ?を定義し、汎用算術演算パッケージに設定せよ。
(load "./sec3.3.3") ;; get, put
(load "./sec2.4.2") ;; attach-tag, contents, etc
(load "./sec2.4.3") ;; install-(rectangular/polar)-package, apply-generic
(load "./sec2.5.1")

(install-scheme-number-package)

; 演算だけのinstall-packageを作るのはどうか、という話。
(define (equ? x y)
  (apply-generic. 'equ? x y))

(put 'equ? '(scheme-number scheme-number) =)
(put 'equ? '(rational rational) equ-rat)




