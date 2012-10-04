;; sec3.4.2.scm からの改良。protectedをいちいちwithdrawとdepositに適用するのではなく、
;; letで変数として定義してしまう考え方。これどうよ？という問題
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
    ;; ここが違ってる。
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))

    (define (dispatch m)
      (cond ((eq? m 'withdraw) protected-withdraw) ;; 呼び出し
            ((eq? m 'deposit) protected-deposit) ;; 呼び出し
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch)))


;; これ, 変数定義まではserializeされてるけど, dispatchで適用されるときはserializeされてない?
;;  => webの回答見てみたらIt's safeだってさー

