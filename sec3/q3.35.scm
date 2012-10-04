;; Ben「Louisたんはだめだなあ」
;;   Benが提唱した基本制約としての平方器を完成させよ。
(load "./sec3.3.5")
(use util.match)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (match request
           ('I-have-a-value (process-new-value))
           ('I-lost-my-value (process-forget-value))
           (else (error "Unknown request" request))))

  (connect a me)
  (connect b me)
  me)

