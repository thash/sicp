(load "./my_defs")
(load "./q3.8")

(use gauche.test)
(test-section "evaluation model")

(eqr (+ (f 1) (f 0)) => 1)

;; ここでfの状態をリセットしたい.. 続けてテストできない
; (eqr (+ (f 0) (f 1)) => 1)
