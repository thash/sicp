;; お金を口座から口座へ移すtransfer処理を考える。
;; Benは、make-accountでdepositとwithdrawをserializeしておけば、あとはtransferであらゆるやりとりのserializeが事足りると考えた。

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;; ところがLousはこんなんいっかだダボがと言う。exchangeの時のような"より巧妙な"方法が必要だと言う。正しいのはどっちだ？

;; なんとなく回答を見て説明しようとしてみる
;; Louseは間違っている。
;; なんとなればtransferは手続きが発動した瞬間からamountが決まっているがexchangeはそうではないから...

