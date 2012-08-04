(load "./my_defs")
(load "./q3.35")

(use gauche.test)
(test-section "celsius-fahrenheit-converter")

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "A" A)
(probe "B" B)

(set-value! A 10 'user)
(forget-value! A 'user)
(set-value! B 9 'user)

;;; output ;;;
;; Probe: A = 10
;; Probe: B = 100
;;
;; Probe: A = ?
;; Probe: B = ?
;;
;; Probe: B = 9
;; Probe: A = 3
