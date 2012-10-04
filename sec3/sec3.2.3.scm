;; 3.2.3. 局所変数の入れ物としてのフレーム

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds.")))

(define W1 (make-withdraw 100))
(W1 50) ;; => 50

;; make-withdrawを引数に作用させたとき、環境モデルに起こる現象が興味深い(らしい)。


