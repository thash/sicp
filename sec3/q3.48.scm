;; http://d.hatena.ne.jp/nrvct/20091221/1261402179
;; 曰く"このへんあんまりおもしろくないです"
;; まあそうだね

(define (make-account balance number)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'number) number)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (cond ((<= (account1 'number) (account2 'number))
           ((serializer1 (serializer2 exchange)) account1 account2))
          ((> (account1 'number) (account2 'number))
           ((serializer2 (serializer1 exchange)) account1 account2)))))

