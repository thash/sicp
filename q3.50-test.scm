(load "./my_defs")
(use gauche.test)

(test-start "q3.50")
(load "./stream")
(load "./q3.50")

(test-section "stream-map | empty")
(define s (stream-map sqrt the-empty-stream))
(display s)
(newline)
(display-stream s)

(test-section "stream-map | enumerate-interval")
(define x (stream-enumerate-interval 50 100))
(define t (stream-map sqrt x))
(display t)
(newline)
(display-stream t)

