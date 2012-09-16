;; Louisの考えた対stream定義
(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

;; ↓ だめー

;; cons-streamがinterleaveをdelaoy objectに包んでくれていたのに、それをむき出しにしてしまったもんだから再帰的評価が走って無限ループになる。




