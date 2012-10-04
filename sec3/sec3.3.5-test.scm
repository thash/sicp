(load "./my_defs")
(load "./sec3.3.5")

;; 9C = 5(F - 32)

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(use gauche.test)
(test-section "celsius-fahrenheit-converter")

(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

;(trace set-value! forget-value!)

(set-value! C 25 'user)
(begin (newline) (print "---------"))
(forget-value! C 'user)
;(forget-value! F 'user) ;; => ignored


