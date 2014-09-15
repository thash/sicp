;; Alyssa「なんでif特殊形式なん? condだけ特殊形式にして，その変化形としてifを定義できんの?」
;; Eva「できるよ」

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ;; 5
(new-if (= 1 1) 0 5) ;; 0

;; うまく動きそうに見える.
;; そこで, これをsec1.1.7のsqrt-iterに適用して次のように書きなおした.

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x ) x)))

;; しかし, 実行するとgoshはフリーズする. なぜか?

;; 確認のためnew-ifを使わず以下のようにcondで実行してみると, 問題なく結果が返る.

(define (sqrt-iter guess x)
  (cond ((good-enough? guess x) guess)
        (else (sqrt-iter (improve guess x ) x))))

;; ifもcondもpredicateを先に評価, それが真なら次のpredicate(あるいはelse)を評価...という順序.
;; となると問題はifとcondの違いにあるわけではない. 問題はnew-ifが手続き定義であること.

;; 回答: new-ifを手続きとして定義したためnormal-orderの引数評価が行われ,
;;       else-clauseに渡した再帰式をがんがん展開してしまうためフリーズする.
