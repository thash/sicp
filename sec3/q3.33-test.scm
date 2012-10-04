(load "./my_defs")
(load "./q3.33")

(use gauche.test)
(test-section "averager")

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)
(probe "A" a)
(probe "B" b)
(probe "Average" c)

(set-value! a 10 'user)
(set-value! b 4 'user)
(begin (newline) (print "---------"))

(forget-value! b 'user)
(begin (newline) (print "---------"))

(set-value! b 22 'user)

