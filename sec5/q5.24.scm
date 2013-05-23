;; > condをifへ簡約するのではなく(without reducing it to if) "新しい基本的特殊形式(a new basic special form)"として実装せよ.
;; > cond節の述語を順に, 真のものを見つけるまでテストするループを構成し,
;; > 次にev-sequenceを使い, その節の行動を評価しなければならない.
;; ev-cond-consequentとev-cond-else-clauseでunevに保存した後なぜsequenceに行くのかわからない
;; unevはun-evaluateなのだろうか. expにあるのが評価対象でそれ以外のなんやかんやが入る.

;; Ref: http://www.serendip.ws/archives/3494
;; based on q5.23.scm 適用後のコード

ev-cond
;; (assign exp (op cond->if) (reg exp)) <- 削除
(assign unev (op cond-clauses) (reg exp)) ; condの本体(cdr exp:1)を取り出してunev:1に保存する
(save continue)
(goto (label ev-cond-loop))

ev-cond-loop
;; condの本体(unev:1)がnull = 該当なしの場合
;; condの書き方によってはelseで拾われず該当なし, となる可能性がある.
(test (op null?) (reg unev))
(branch (label ev-cond-null))
;; unev:1には(初回:condの本体が/二回以降:残りの節が)入っているので
;;           (初回:最初の/二回以降:次の)節をexp:2に保存
(assign exp (op car) (reg unev))
(test (op cond-else-clause?) (reg exp)) ; そのassignしたexp:2がelseであったばやい
(branch (label ev-cond-else-clause))
(save exp) ; exp:2 (= (car unev:1))をstackに退避
(assign exp (op cond-predicate) (reg exp)) ; 述部(car exp:2)をexp:3に保存
(save unev) ; unev:1をstackに退避
(save env)
(assign continue (label ev-cond-decide))
(goto (label eval-dispatch))

;; eval-dispatchで評価したvalを用いて
;; もうゴールしてもいいよね or まだloopするのかをdecideする
ev-cond-decide
(restore env)
(restore unev) ; unev:1
(restore exp) ; exp:2
(test (op true?) (reg val)) ; その節=exp:3をeval-dispatchで評価した結果がtrueだった場合
(branch (label ev-cond-consequent))
(assign unev (op cdr) (reg unev)) ; unev:1を減らして(= unev:1のcdr部を)unev:2に保存
(goto (label ev-cond-loop))

ev-cond-consequent
(assign unev (op cond-actions) (reg exp)) ; (cdr exp:2) をunev:3に保存
(goto (ev-sequence))

ev-cond-else-clause
(assign unev (op cond-actions) (reg exp)) ; (cdr exp:2) をunev:4に保存
(goto (label ev-sequence))

ev-cond-null
(restore continue)
(assign val (const #f)) ; 結果はfalse. ここは言語仕様次第か.
(goto (reg continue))


(define eceval-operations
  ;;...
  ;; (cond->if ,cond->if) <- 削除
  (cond-clauses ,cond-clauses)
  (cond-else-clause? ,cond-else-clause?)
  (cond-predicate ,cond-predicate)
  (cond-actions ,cond-actions)
  ;;...
  )

;; ちなみに中身はcar/cdr, else表記のチェック程度
;; cond-clauses => (cdr exp))
;; cond-else-clause? => (eq? (cond-predicate clause) 'else)
;; cond-predicate => (car clause)
;; cond-actions => (cdr clause)

