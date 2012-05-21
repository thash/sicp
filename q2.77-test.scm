(load "./my_defs")
(load "./q2.77")

(use gauche.test)

(test-start "q2.77")
(test-section "make-complex-from-real-imag")
(eqr (make-complex-from-real-imag 1 2)
     => '(complex rectangular 1 . 2))


(test-section "invalid magnitude")
(define z (make-complex-from-real-imag 1 2))
; how to expect error?
; (eqr (magnitude z)
;      => ')

; after install additional put...
(install-additional-complex)
(eqr (magnitude z)
     => '2.23606797749979)


(test-section "trace inside magnitude")
(trace magnitude)
(trace apply-generic)
(trace get)
(trace put)

(eqr (magnitude z)
     => '2.23606797749979)

; 1. apply-generic magnitude (complex rectangular 1 . 2)
; 2. apply-generic magnitude (rectangular 1 . 2)
;   と、2かい呼び出されている。


