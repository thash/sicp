(load "./my_defs")
(load "./q3.7")

(use gauche.test)
(test-section "make-account with joint")


(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

;(trace make-joint)
(eqr ((peter-acc 'open-sesame 'withdraw) 40) => 60)
(eqr ((paul-acc 'rosebud 'withdraw) 30) => 30)
(eqr ((paul-acc 'open-sesame 'withdraw) 30) => (test-error))
(eqr ((peter-acc 'rosebud 'withdraw) 10) => (test-error))

