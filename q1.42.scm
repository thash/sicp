(add-load-path ".")
(load "my_defs")
(define (conpose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))
;(print
;((compose square inc) 6)
;)
