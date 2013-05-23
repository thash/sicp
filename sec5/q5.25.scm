;; 評価器を修正し, 4.2節の遅延評価器に基づいた正規順序の評価が使えるようにせよ.
;; Modify the evaluator so that it uses normal-order evaluation, based on the lazy evaluator of section 4.2.

;; 1. normal-order evaluationをするためにはthunkをcacheすべきではない.
;; 2. この目的を達成するだけならev-map-operandsという手続きを用意すれば事足りる.

;; ref: https://github.com/skanev/playground/blob/master/scheme/sicp/05/25.scm

;; > 遅延世界ではif述語部分のみ, evalの代わりにactual-valueを使う.

;; レジスタ計算機本体
(load "./sec5.2-A-Register-Machine-Simulator.scm")

;; 4章の評価器(遅延ver)
(load "./sec4/sec4.2-lazy")
(define (announce-output arg) (annouce-output arg))

;; 遅延がらみ手続き追加
(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; opsに加える
(define lazy-operations
  `((delay-it ,delay-it)
    (thunk? ,thunk?)
    (thunk-exp ,thunk-exp)
    (thunk-env ,thunk-env)))

;; 命令列本体に追加
;; ...
delay-it
(assign val (op delay-it) (reg exp) (reg env))
(goto (reg continue))

;; force-it-thunkからここへ来る. expには遅延処理本体が入ってる
;; continueだけ修正してeval-dispatchへ飛ばす
actual-value
(save continue)
(assign continue (label actual-value-after-eval))
(goto (label eval-dispatch))
actual-value-after-eval ; eval-dispatchを経験しvalに結果が入った状態で戻ってくる
(restore continue)
(assign exp (reg val))
(goto (label force-it))

force-it
(test (op thunk?) (reg exp))
(branch (label force-it-thunk))
(goto (reg continue)) ; thunkを解凍しおわったらもとのcontinueへ
force-it-thunk ; ここに来るってことはexpはthunk
(assign env (op thunk-env) (reg exp))
(assign exp (op thunk-exp) (reg exp)) ; expを遅延処理本体で上書き
(goto (label actual-value))


;; 修正する箇所(目grepした範囲なので抜けあるかも)
primitive-apply
  (assign exp (label actual-value)) ; expに"label"を代入?
  (assign continue (label primitive-apply-after-args))
  (goto (label ev-map-operands))
primitive-apply-after-args
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (save continue)
  (assign exp (label delay-it))
  (assign continue (label compound-apply-after-args))
  (goto (label ev-map-operands))
compound-apply-after-args
  (restore continue)
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label actual-value)) ;; 変更

;; ev-map-operandsシリーズ(大きな追加)
ev-map-operands
  (assign argl (op empty-arglist))
  (test (op no-operands?) (reg unev))
  (branch (label ev-map-no-args))
  (save continue)
  (save proc)
  (assign proc (reg exp))
  (save proc)
ev-map-operand-loop
  (restore proc)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-map-last-arg))
  (save proc)
  (save argl)
  (save env)
  (save unev)
  (assign continue (label ev-map-accumulate-arg))
  (goto (reg proc))
ev-map-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-map-operand-loop))
ev-map-last-arg
  (save argl)
  (assign continue (label ev-map-accumulate-last-arg))
  (goto (reg proc)) ; procにlabel?わけわからん
ev-map-accumulate-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (goto (reg continue))
ev-map-no-args
  (goto (reg continue))


