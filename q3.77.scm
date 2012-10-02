;; sec3.5.4.scm 本文中で実装したintegralを, 別の方法で実装する.
(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                 the-empty-stream
                 (integral (stream-cdr integrand)
                           (+ (* dt (stream-car integrand))
                              initial-value)
                           dt))))

;; d2y/dt2 + a * cy/dt - b * y = 0

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

