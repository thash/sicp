(load "./my_defs")
(load "./q3.17")

(use gauche.test)
(test-start "count-pairs")

(test-section "3 pairs, looks 4")
(define l1 (list 'a))
(define l2 (list 'b 'c))
(define l3 (list 'a 'b 'c))
(set-car! l2 l1)
(set-car! (cdr l2) l1)

(eqr (count-pairs l2) => 3)

(test-section "3 pairs, looks 7")
(define x1 (list 'a))
(define x2 (list 'b))
(define x3 (list 'c))

(set-car! x1 x2)
(set-cdr! x1 x2)
(set-car! x2 x3)
(set-cdr! x2 x3)

(eqr (count-pairs x1) => 3)

(test-section "3 pairs, infinity loop")
(define x-inf (list 'a 'b 'c))
(set-cdr! (cdr (cdr x-inf)) x-inf)

;; やっぱ無限ループじゃないですかーやだー
;; (eqr (count-pairs x-inf) => 3)

