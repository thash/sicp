(add-load-path ".")
(load "q2.7")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


; gosh> (define i3 (make-interval 1 2))
; gosh> (define i4 (make-interval 0 4)
; gosh> (sub-interval i4 i3) ; => (-2 . 3)
; gosh>  (sub-interval i3 i4) ; => (-3 . 2)


