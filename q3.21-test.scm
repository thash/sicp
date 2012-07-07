(load "./my_defs")
(load "./q3.21")

(use gauche.test)
(test-section "print-queue")

(define q1 (make-queue))
(eqr (print-queue (insert-queue! q1 'a)) => '(a))
(eqr (print-queue (insert-queue! q1 'b)) => '(a b))
(eqr (print-queue (delete-queue! q1))    => '(b))
(eqr (print-queue (delete-queue! q1))    => '())

