(load "./my_defs")
(load "./q3.25")

(use gauche.test)

(test-section "1D table")

(define t1 (make-table))
(define get1 (t1 'lookup-proc))
(define put1 (t1 'insert-proc!))
(put1 '(a) 2.0)
(eqr (get1 '(a)) => 2.0)


(test-section "2D table")
(define t2 (make-table))
(define get2 (t2 'lookup-proc))
(define put2 (t2 'insert-proc!))
(put2 '(a b) 9.0)
(eqr (get2 '(a b)) => 9.0)
(put2 '(b y) 22)
(eqr (get2 '(a x)) => #f)
(put2 '(a b) 99.0)
(eqr (get2 '(a b)) => 99.0)

(test-section "4D table")
(define t4 (make-table))
(define get4 (t4 'lookup-proc))
(define put4 (t4 'insert-proc!))
(put4 '(a b c d) 1.0)
(eqr (get4 '(a b c d)) => 1.0)
(eqr (get4 '(a b c)) => '((d . 1.0)))
(eqr (get4 '(a b)) => '((c (d . 1.0))))
(eqr (get4 '(a)) => '((b (c (d . 1.0)))))

