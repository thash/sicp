;; 3.2.4. 内部手続

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; 手続きsqrtは, 内部手続きgood-enough?, improve, sqrt-iterを持つ。
;;
;; TODO: 図3.11

