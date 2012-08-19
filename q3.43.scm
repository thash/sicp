;; 3つの口座の初めの残高が10, 20, 30で、口座の残高をexchangeしながら複数プロセスが走るとする。

;; (1). プロセスが逐次的に走ったなら、任意の回数の交換の後で10,20,30となっているべきことを論ぜよ。
;; (2). 図3.29のようなタイミング図を描き、本節の口座交換プログラム(exchange)の最初のversionを使って交換すると(1)の条件が破られることを示せ。
;; (3). 最初verのexchangeを使っても残高の合計は保存されることを論ぜよ。
;; (4). 各口座の直列化すらやめてしまえば(3)の条件さえも破れることを示せ。

;; まとめると以下のような関係性を示せばよい



;; ちなみに(2)で言う「本節の口座交換プログラム(exchange)の最初のversion」とは
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; これのことだろう。

