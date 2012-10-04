;; 9C = 5(F - 32)
;; 式をそのまま書き下すような形で定義したい。

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

;; c+やc*は算術演算の"制約版(原文: constraint version)"であり、次のように定義される。
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;; 問題: 残りのc*, c/, cv(定数値)を定義せよ.
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

