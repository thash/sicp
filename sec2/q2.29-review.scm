(add-load-path "./../")
(load "my_defs")

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (if (= (torque (left-branch mobile))
         (torque (right-branch mobile)))
    #t
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(define m1 (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define m2 (make-mobile (make-branch 2 3) (make-branch 4 5)))
(define m3 (make-mobile (make-branch 2 m1) (make-branch 3 m2)))

(eqr m1 => '((2 3) (2 3)))
(eqr (left-branch m2) => '(2 3))
(eqr (right-branch m2) => '(4 5))
(eqr (branch-length (left-branch m2)) => 2)
(eqr (branch-structure (left-branch m2)) => 3) ; weight
(eqr (branch-structure (left-branch m3)) => '((2 3) (2 3))) ; anothe mobile
(eqr (branch-structure (left-branch m3)) => m1) ; ... it's m1.
(eqr (total-weight m1) => 6)
(eqr (total-weight m3) => 14)
(eqr (torque (right-branch m1)) => 6)
(eqr (torque (right-branch m2)) => 20)
(eqr (balanced? m1) => #t)
(eqr (balanced? m2) => #f)

