(load "./my_defs")  ;; trace, etc
(load "./sec3.3.3") ;; get, put
(load "./sec2.4.2") ;; attach-tag, contents, etc
(load "./sec2.4.3") ;; install-(rectangular/polar)-package, apply-generic
(load "./sec2.5.1")

;; install packages. complex package is depends on rectangular/polar packages
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


;; complex packageに以下のものを追加しないといけない。なぜか。
(define (install-additional-complex)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle))


; 以下の式を評価するとき呼び出される全ての手続きをトレースせよ。apply-genericは何度呼び出されるか。
; (magnitude z)
;  => test参照. 2回



