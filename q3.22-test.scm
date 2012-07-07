(load "./my_defs")
(load "./q3.22")

(use gauche.test)
(define q (make-queue))

(eqr (print-queue q) => (test-error))
(insert-queue! q 'a)
(eqr (print-queue q) => '(a))
(insert-queue! q 'b)
(eqr (print-queue q) => '(a b))
(delete-queue! q)
(eqr (print-queue q) => '(b))
(delete-queue! q)
(eqr (print-queue q) => (test-error))

;; 空のqueueに対して呼ぶとerrorになる

