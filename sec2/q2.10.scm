(add-load-path ".")
(load "sec2.1.4")

; from sec2.1.4
; (define (div-interval x y)
;   (mul-interval x
;                 (make-interval (/ 1.0 (upper-bound y))
;                                (/ 1.0 (lower-bound y))
;                                )))

(define (div-interval x y)
  ; yの区間がゼロをまたがるとき、符号が違うのでカケたら負になる
  (if (< (* (lower-bound y) (upper-bound y)) 0)
    (error "error")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))
                                 ))))

