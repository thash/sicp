(load "./sec4.3-nondeterministic")

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))


;; how to use
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 1 20)

;;; Amb-Eval input:
;;; Starting a new problem
;;; Amb-Eval value (3 4 5)
;;; Amb-Eval input: try-again
;;; Amb-Eval value (5 12 13)
;;; Amb-Eval input: try-again
;;; Amb-Eval value (6 8 10)
;;; Amb-Eval input: try-again
;;; Amb-Eval value (8 15 17)
;;; Amb-Eval input: try-again
;;; Amb-Eval value (9 12 15)
;;; Amb-Eval input: try-again
;;; Amb-Eval value (12 16 20)
