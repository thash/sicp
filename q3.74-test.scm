(load "./my_defs")
(prepare-test)

;; (sign-change-detector current last)
(eqr (sign-change-detector 1 -1) =>  1) ; -1 to  1
(eqr (sign-change-detector -1 1) => -1) ;  1 to -1
(eqr (sign-change-detector 0  1) => -1) ;  1 to  0
(eqr (sign-change-detector 0 -1) =>  1) ; -1 to  0
(eqr (sign-change-detector -1 0) =>  0) ;  0 to -1
(eqr (sign-change-detector -1 0) =>  0) ;  0 to  1

; (display-stream-n zero-crossings 10)
; 0.84,  0.90,  0.14, -0.75, -0.95, -0.276,  0.65,  0.98,  0.41,
; 0, 0, 1, 0, 0, -1, 0, 0, 1,

