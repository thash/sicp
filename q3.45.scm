;; Louis によれば、いままで作った銀行口座システムはdepositとwithdrawが自動的にserializeされないので複雑になるしerror porneだという。
;; 彼は(ルイズたん男なの...)口座(balance?)とdepositを直列化し、*さらにそれに加えて* make-account-and-serializerが直列変換器を輸出(?)すべきであるという。
;; Louisの提唱する口座定義は以下の通り。

(define (make-account-and-serializer balance)
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
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; で、これを使えば当初のdeposit/withdrawの使い方で良いじゃんという。
(define (deposit account amount)
  ((account 'deposit) amount))

;; Louisの考えで何が悪いか？
;; serialized-exchangeを呼び出したとき何が起きるか。


