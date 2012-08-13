;; sec3.4.2.scm からさらにbalanceの参照をprotectedで直列化したver.
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             (protected (lambda () balance))) ;; ここが違う。直列化した
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


;; > I think Ben’s concern is unnecessary since both withdraw and deposit have been protected. It is possible that the value returned by checking balance may not be accurate. But this should be an expected behavior. If we use Ben’s approach, checking balance will return an accurate balance. However, it is only accurate at the moment of returning. So Ben’s approach is not really an improvement.

;; 直列化が必要になるのは値を書き換える時であり、balaneの参照を直列化してもあんま意味ない。

