
;; integrand
(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                 the-empty-stream
                 (integral (stream-cdr integrand)
                           (+ (* dt (stream-car integrand))
                              initial-value)
                           dt))))


;; d2y/dt2 + a * cy/dt - b * y = 0


