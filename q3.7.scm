;; 共同口座を作る手続き。中でmake-accountしちゃだめか...別物になってしまう。
;; 異なるパスワードでのアクセスだけ許可する。
;; ref: http://wizardbook.wordpress.com/2010/12/14/exercise-3-7/
(define (make-joint acc base-pass new-pass)
  (define (dispatch password m)
    (cond ((not (eq? password new-pass))
           (error "Incorrect password. -- MAKE-JOINT"))
          ((eq? m 'withdraw) (acc base-pass 'withdraw))
          ((eq? m 'deposit) (acc base-pass 'deposit))
          (else (error "Unknown request -- MAKE-JOINT" m))))
 ; (trace dispatch)
  dispatch)

;; q3.3.scm で作成した make-account.
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (not (eq? password p))
      (error "Incorrect password." p)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))))
  dispatch)

