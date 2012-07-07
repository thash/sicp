(load "./my_defs")

(use gauche.test)
(test-section "assoc")

(define z '((a . b) (c . d) (e . f)))

;; gosh> z
;; ((a . b) (c . d) (e . f))
;; gosh> (assoc 'e z)
;; (e . f)

(eqr (assoc 'a z) => '(a . b))
(eqr (assoc 'x z) => #f)
(eqr (assv 'a z) => '(a . b))
(eqr (assq 'a z) => '(a . b))


