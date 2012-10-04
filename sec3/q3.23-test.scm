(load "./my_defs")
(load "./q3.23")

(use gauche.test)
(define q (make-deque))

(front-insert-queue! q 'a)
(print-queue q)
(front-insert-queue! q 'b)
(print-queue q)
(front-delete-queue! q)
(print-queue q)
(front-insert-queue! q 'b)
(print-queue q)
(rear-delete-queue! q)
(print-queue q)


