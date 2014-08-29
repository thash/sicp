;; 局所状態変数 -- letではない

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) (display "通報しますた"))
  (define miss-count 0)
  (define (dispatch p m)
    (if (not (eq? password p))
      (begin 
        (set! miss-count (+ miss-count 1))
        (if (= miss-count 7)
          (begin (call-the-cops) (set! miss-count 0))
          (error "Incorrect password." p)))
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))))
  dispatch)


