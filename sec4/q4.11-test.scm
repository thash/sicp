(load "./my_defs")
(prepare-test)

(define vars '(a b c))
(define vals '(1 2 3))

(eqr (make-frame vars vals) => '((a . 1) (b . 2) (c . 3)))

(define f (make-frame vars vals))
(eqr (frame-variables f) => '(a b c))
(eqr (frame-values f) => '(1 2 3))

(eqr (add-binding-to-frame! 'd 4 f) => '((a . 1) (b . 2) (c . 3) (d . 4)))




