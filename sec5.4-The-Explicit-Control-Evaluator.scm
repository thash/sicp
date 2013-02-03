;;;   5.4 積極制御評価器
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;     5.4.1 積極制御評価器の中核
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 手続きはlabelのように表せる.
;; eval-dispatchは4章(sec4.1)のevalに相当.
;; copyしてきて記述をレジスタ計算機形式に変更
eval-dispatch
(test (op self-evaluating?) (reg exp))
(branch (label ev-self-eval))
(test (op variable?) (reg exp))
(branch (label ev-variable))
(test (op quoted?) (reg exp))
(branch (label ev-quoted))
(test (op assignment?) (reg exp))
(branch (label ev-assignment))
(test (op definition?) (reg exp))
(branch (label ev-definition))
(test (op if?) (reg exp))
(branch (label ev-if))
;; (test (op let?) (reg exp)) 本文には記述なし => 問題5.23
;; (branch (label ev-let))
(test (op lambda?) (reg exp))
(branch (label ev-lambda))
(test (op begin?) (reg exp))
(branch (label ev-begin))
;; (test (op cond?) (reg exp)) 本文には記述なし => 問題5.23
;; (branch (label ))
(test (op application?) (reg exp))
(branch (label ev-application))
(goto (label unknown-expression-type))


;; 単純式の評価
ev-self-eval
(assign val (reg exp))
(goto (reg continue))
ev-variable
(assign val (op lookup-variable-value) (reg exp) (reg env))
(goto (reg continue))
ev-quoted
(assign val (op text-of-quotation) (reg exp))
(goto (reg continue))
ev-lambda
(assign unev (op lambda-parameters) (reg exp))
(assign exp (op lambda-body) (reg exp)) ;; expを上書きしてる
(assign val (op make-procedure) (reg unev) (reg exp) (reg env))
(goto (reg continue))

;; > ev-lambdaがenvの環境とともにlambda式のパラメタと本体をmake-procedure演算に渡せるように,
;; > それらをunevとexpレジスタでどう保持するか見てほしい.

;; ev-applicationは4章eval中でcond分岐したところで実行されるapplyに相当
ev-application
(save continue)
(save env)
(assign unev (op operands) (reg exp))
(save unev)
(assign exp (op operator) (reg exp))
(assign continue (label ev-appl-did-operator))
(goto (reg eval-dispatch)) ; 最後のgotoがcontinueじゃない!

ev-appl-did-operator
(restore unev) ; 被演算子
(restore env)
(assign argl (op empty-arglist))
(assign proc (reg val))
(test (op no-operands?) (reg unev))
(branch (label apply-dispatch)) ; evalではなくapplyのdispatch
(save proc)

;; 被演算子を評価し, 結果をarglに格納する.
ev-appl-operand-loop
(save argl)
(assign exp (op first-operand) (reg unev))
(test (op last-operand?) (reg unev)) ; 最後の引数は特別に扱う.
(branch (label ev-appl-last-arg)) ; lyを省略することにどれほど意味があるのか
(save env)
(save unev)
(assign continue (lavel ev-appl-accumulate-arg))
(goto (label eval-dispatch))

;; arglへ評価後の引数を蓄積.
;; unevから未評価引数が1個外される.
ev-appl-accumulate-arg
(restore unev)
(restore env)
(restore argl)
(assign argl (op adjoin-arg) (reg val) (reg argl))
(assign unev (op rest-operands) (reg unev))
(goto (label ev-appl-operand-loop))

ev-appl-last-arg ; 特別に扱う最後の引数
(assign continue (label ev-appl-accum-last-arg))
(goto (label eval-dispatch))
ev-appl-accum-last-arg
(restore argl)
(assign argl (op adjoin-arg) (reg val) (reg argl))
(restore proc)
(goto (label apply-dispatch))

apply-dispatch
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-apply))
(test (op compound-procedure?) (reg proc))
(branch (label compound-apply))
(goto (label unknown-procedure-type))

primitive-apply
(assign val (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
(restore continue)
(goto (reg continue))

;; 合成手続きの作用は超循環評価器と同様
compound-apply
(assign unev (op procedure-parameters) (reg proc))
(assign env (op procedure-environment) (reg proc))
(assign env (op extend-environment)
            (reg unev) (reg argl) (reg env)) ; 上書き?
(assign unev (op procedure-body) (reg proc)) ; また
(goto (label ev-sequence))


;;;     5.4.2 並びの評価と末尾再帰
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ev-beginはeval-dispatchの生き残り
ev-begin
(assign unev (op begin-actions) (reg exp))
(save continue)
(goto (label ev-sequence))

ev-sequence
;; [末尾再帰] まず全部評価してから最後かどうか判定してる.
(assign exp (op first-exp) (reg unev))
(test (op last-exp?) (reg unev))
(branch (label ev-sequence-last-exp))
(save unev)
(save env)
(assign continue (label ev-sequence-continue))
(goto (label eval-dispatch))
ev-sequence-continue
(restore env)
(restore unev)
(assign unev (op rest-exps) (reg unev))
(goto (label ev-sequence))
ev-sequence-last-exp
(restore continue)
(goto (label eval-dispatch)) ; [末尾再帰] continueではなくeval-dispatchに戻るよ!

;;; 末尾再帰 -- Tail recursion ;;;
;; うえでやったような, 情報をスタックに退避せず直接eval-dispatchに行く評価器は"末尾再帰的".
;; 末尾再帰をしないこんな実装も出来る.
yet-another-ev-sequence
  (test (op no-more-exps?) (reg unev)) ; 最初にチェック
  (branch (label yet-another-ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label yet-another-ev-sequence-continue))
  (goto (label eval-dispatch))
yet-another-ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label yet-another-ev-sequence))
yet-another-ev-sequence-end
  (restore continue)
  (goto (reg continue)) ; *

;; > ...this change is fatal to the tail-recursive implementation,
;; > because we must now return after evaluating the final expression in a sequence in order to undo the (useless) register saves.
;; > These extra saves will accumulate during a nest of procedure calls.


;; eval-dispatchの生き残りふたたび
ev-if
(save exp)
(save env)
(save continue)
(assign continue (label ev-if-decide))
(assign exp (op if-predicate) (reg exp))
(goto (reg eval-dispatch)) ; continueではない
ev-if-decide
(restore continue)
(restore env)
(restore exp) ; restoreの順序大切
(test (op true?) (reg val))
(branch (label ev-if-consequent))
ev-if-alternative
(assign exp (op if-alternative) (reg exp))
(goto (label eval-dispatch))
ev-if-consequent
(assign exp (op if-consequent) (reg exp))
(goto (label eval-dispatch))


;; eval-dispatchの生き残りラスト. 代入系
;; 代入はregisterより上層のお話なので頭混乱しないように...
ev-assignment
(assign unev (op assignment-variable) (reg exp))
(save unev) ; 後のために変数を退避 (変数だったのか...)
(assign exp (op assignment-value) (reg exp))
(save env)
(save continue)
(assign continue (label ev-assignment-1))
(goto (label eval-dispatch))
ev-assignment-1
(restore continue)
(restore env)
(restore unev)
(perform
  (op set-variable-value!) (reg unev) (reg val) (reg env))
(assign val (const ok))
(goto (reg continue))

;; definitionも基本assignmentと同じ
ev-definition
(assign unev (op definition-variable) (reg exp))
(save unev)
(assign exp (op definition-value) (reg exp))
(save env)
(save continue)
(assign continue (label ev-definition-1))
(goto (label eval-dispatch))
ev-definition-1
(restore continue)
(restore env)
(restore unev)
(perform
  (op define-variable!) (reg unev) (reg val) (reg env)) ; op名違うだけ
(assign val (const ok))
(goto (reg continue))

;; => q5.23.scm, q5.24.scm, q5.25.scm

