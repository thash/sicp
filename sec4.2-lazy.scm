;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. 超言語的抽象(metalinguistic abstraction)
;;
;; =============================================================================
;; プログラム言語で式の意味を決定する評価器は, もう一つのプログラムに過ぎない.
;; =============================================================================
;;
;; ほとんどのプログラムは, ある言語の評価器(evalator)と見ることが出来る.
;; 4章ではLispの手続きとしてevalatorを実装する.
;;   4.2. 正規評価順序(normal-order evaluation)を可能にする
;;   4.3. 非決定性計算(nondeterministic computing)
;;        -- 多値 ... 式は唯一ではなく, 多くの値が取れるようになる.
;;   4.4. 論理型プログラミング(logic programming)
;;        -- "関係を使って知識を表現する(knowledge is expressed in terms of relations)"
;;            Prologのような?
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1. 超循環評価器(The Metacircular Evaluator)
;;
;;  評価する言語と同じ言語で書かれた評価器を超循環評価器という
;;
;;  > 合成手続きを一組の引数に作用させるには, 手続き本体を新しい環境で評価する.
;;  > この環境を構成するには, 手続きオブジェクトの環境部分を, 手続きの各パラメタが, 手続きを作用させる引数に束縛されるフレームで拡張する.
;;
;;  Evaluatorの実装は, 評価される式の構文(syntax)を定義する手続きに依存する. Evaluatorを実装する手続きと名前が被らないように,
;;  代入をset!ではなくassignment?, assignment-variableなどで操作するようにするなど気をつける.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.1. 評価器の中核

;; Gaucheのtrue/falseが後々混乱を呼ぶので最初に定義.
(define true #t)
(define false #f)

;;  評価プロセスはevalとapplyの間の相互作用として記述できる.

;; apply
;;   applyは引数として"手続き"と, "手続きを作用させる引数のリスト"を取る.
;;   > eval の定義を apply の定義よりも先に行っていたために、eval の定義の中で使っていた apply 手続きが Gauche のシステムの apply 手続きを利用していたために動作しなかったらしい。 (ref: http://www.serendip.ws/archives/1817)
;;   という情報があったためapplyを上に持ってきた.

;; primitive proceduresを評価するとき素applyが欲しいので残しとく(p.227 脚注17参考).
(define apply-in-underlying-scheme apply)

;; evalはapplyに, 評価されていない被演算子の式argumentsを渡している.
;; envを受け取るようにしつつ, argumentsの扱いに気をつけなければならない.
;;   * primitive-procedureを作用させるときはlist-of-arg-values
;;   * extend-environmentの時はdelayed-objectのargsを渡す
;; ちなみにprocedureはactual-valueでforceされているので評価済.
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

;;  eval
;;    evalは引数として"式"と"環境"をとる. 式を分類して評価を振り分ける.
;;    evalの中身は式に応じた場合分けの形を取る.

;; 注意: 4.1.7. にてanalyze版に上書きされる
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))



;; list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;; eval-if
;; 遅延世界ではif述語部分のみ, evalの代わりにactual-valueを使う.
;; 他のふたつ(true/falseそれぞれの時の本体)はevalで良い.
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;; eval-sequence
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; eval-assignment (代入)
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;; eval-definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; => q4.1.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.2. 式の表現
;;
;;   実装する構文の仕様は
;;     * 自己評価式は数と文字列のみ
;;     * 変数は記号で表現
;;     * クォート式は(quote <text-of-quotation>)の形
;;     * 代入は(set! <var> <value>)の形
;;     * 定義は(define <var> <value>)または(define (<var> <parameters...>) <body>)の形(後者はsyntax sugar)
;;     * lambda式は記号lambdaで始まるリスト
;;     * 条件式はifではじまり, then部とelse部(else部がなければfalse)を持つ
;;     * beginは式の並びを単一の式に包み込む
;;     * 手続き作用は上記のいずれにも合致しない任意の合成式
;;   淡々と書いていくよ

;; 自己評価式は数と文字列だけ(数やら文字列とは, という定義はunderlyingなschemeに任せてる)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;; 代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

;; set! に相当する手続きで使われてるぽい.
;; (proc arg1...)という形式があるdefineとは異なり第一引数が必ずsymbolである.
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; pairだったらなんでもapplicationなので順序に注意. この定義はq4.2.scmで突っ込まれる.
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; "導出された式(derived expression)"の一例, cond.
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; condをifの入れ子として評価する.
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))


;; => q4.2.scm, q4.3.scm, q4.4.scm, q4.5.scm, q4.6.scm, q4.7.scm, q4.8.scm, q4.9.scm, q4.10.scm
;;    ここまで2012-10-15予習


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.3. 評価器のデータ構造
;;   環境や手続き, true/falseの表現など
;;   評価器が内部的に使うデータ構造を定義する.

;; 評価器のtrue/false
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;; 基本手続きを扱う以下の手続きが使えるとする.
;;     (apply-primitive-procedure <proc> <args>)
;;     (primitive-procedure? <proc>)
;; 実装は 4.1.4. にて.
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;; 環境に対する操作
;;   評価器は環境を操作する演算を必要とする. 環境はフレームの並びであり, 各フレームは変数を対応する値に対応付ける束縛の表である.
;;   他の方法で環境を表現することもできる => q4.11.scm

(define (enclosing-environment env) (cdr env)) ;; 環境を"狭める" => ただのcdr
(define (first-frame env) (car env))
(define the-empty-environment '()) ;; 空の環境はただの空リスト

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; 外側の環境をbase-envとし, variablesがvaluesに束縛された新しいフレームからなる環境を返す.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

;; 環境envの中で記号varに束縛された値を返す. 中から外へ探す.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;; lookupするだけでなく見つかったら代入する. 走査処理を一般化できそう.
(define  (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; let (q4.6.scm) {{{

(define (let? exp) (tagged-list? exp 'let))
(define (let-assignment exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-exp assignment)
  (if (null? assignment)
    '()
    (cons (cadr (car assignment))
          (let-exp (cdr assignment)))))

(define (let-var assignment)
  (if (null? assignment)
    '()
    (cons (car (car assignment))
          (let-var (cdr assignment)))))

(define (transform-let assignment body)
  (cons (make-lambda (let-var assignment) body)
        (let-exp assignment)))

(define (let->combination exp)
  (transform-let (let-assignment exp) (let-body exp)))
;; }}} let -- from q4.6.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.4. 評価器をプログラムとして走らせる

;; 基本型かどうかは primitive tagが付いているかどうかで判別
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

;; これは本文の進行に応じて増やしていく.
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'display display)
        (list 'newline newline)
        (list 'set! set!)
        ;; (list 'map map)  ;; q4.14.scm でLousがここにmapを加えようとするが, 加えると動かない, らしい(再現できず)
        ;; ....
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; 本文中の順序とは異なるが,
;; setup-environmentはprimitive-procedure-* の後に置かないと未定義エラーが出る
(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; 基本手続きを作用させる
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

;; loop
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value")

(use gauche.time)
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (time (actual-value input the-global-environment))))
      (annouce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (annouce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           最低限動かすならココマデ               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.5. プログラムとしてのデータ
;;
;;  プログラムを抽象的な機械の記述であるとする視点がある.
;;  他の機械がLispプログラムとして記述できれば, それを真似ることができる万能機械である.

;; 以下の2式はどちらも同じ結果(25)を返す.
;    (eval '(* 5 5) user-initial-environment)
;    (eval (cons '* (list 5 5)) user-initial-environment)

;; => q4.15.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.6. 内部定義
;;
;;  define内でさらにdefineする内部定義
;    (define (f x)
;      (define (even? n)
;        (if (= n 0)
;          #t
;          (odd? (- n 1))))
;      (define (odd? n)
;        (if (= n 0)
;          #f
;          (even? (- n 1))))
;          ;....)
;
;; 逐次定義と同時定義の扱いは処理系によって違う.
;; "内部で定義した名前が真に同時有効範囲を持つように定義を扱う"
;;   => lambda式による構文変換
;    (lambda <vars>
;      (define u <e1>)
;      (define v <e2>)
;      <e3>)
;; この手続きは, 以下のように書き換えられる.
;    (lambda <vars>
;      (let ((u '*unassigned*)
;            (v '*unassigned*))
;        (set! u <e1>)
;        (set! v <e2>)
;        <e3>))
;; 最初に*unassigned*で定義してしまい, その後setする流れ. 同時定義とみなせる(?).

;; => q4.16.scm, q4.17.scm, q4.18.scm

;; 以下の2式はどちらも同じ結果(25)を返す.
;    (eval '(* 5 5) user-initial-environment)
;    (eval (cons '* (list 5 5)) user-initial-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.7. 構文解析を実行から分離する
;;
;;  構文解析と実行を同時に行うとプログラムが何回も解析され効率が悪い.
;;  evalから構文解析analyzeを切り出す.

;; 注意: sec4.scm全体をloadした場合, 冒頭のevalを上書きする.
; (define (eval exp env)
;   (analyze exp) env)
;
; (define (analyze exp)
;   (cond ((self-evaluating? exp)
;          (analyze-self-evaluating exp))
;         ((quoted? exp) (analyze-quoted exp))
;         ((variable? exp) (analyze-variable exp))
;         ((assignment? exp) (analyze-assignment exp))
;         ((definition? exp) (analyze-definition exp))
;         ((if? exp) (analyze-if exp))
;         ((lambda? exp) (analyze-lambda exp))
;         ((begin? exp) (analyze-sequence (begin-actions exp)))
;         ((cond? exp) (analyze (cond->if exp)))
;         ((application? exp) (analyze-application exp))
;         (else
;           (error "Unknown expression type -- ANALYZE" exp))))

;;;  次に, analyze内で利用されている以下の手続きを定義していく.
;;;  基本的には実行フェイズに実行されるlambda式を返す.
;;;  その前にできることはしておく.
;; analyze-self-evaluating
;; analyze-quoted
;; analyze-variable
;; analyze-assignment
;; analyze-definition
;; analyze-if
;; analyze-lambda
;; analyze-sequence
;; analyze-application

;; 環境引数を無視して式を実行
;    (define (analyze-self-evaluating exp)
;      (lambda (env) exp))
;
;    (define (analyze-quoted exp)
;      (let ((qval (text-of-quotation exp)))
;        (lambda (env) qval)))
;
;    ;; 変数探索は実行フェイズに行わなければならない.
;    (define (analyze-variable exp)
;      (lambda (env) (lookup-variable-value exp env)))
;
;    ;; assignmentとdefinitionは1度だけ解析すれば良い.
;    (define (analyze-assignment exp)
;      (let ((var (assignment-variable exp))
;            (vproc (analyze (assignment-value exp))))
;        (lambda (env)
;          (set-variable-value! var (vproc env) env)
;          'ok)))
;
;    (define (analyze-definition exp)
;      (let ((var (definition-variable exp))
;            (vproc (analyze (definition-value exp))))
;        (lambda (env)
;          (define-variable! var (vproc env) env)
;          'ok)))
;
;    ;; if は解析時に条件文, trueの時, elseの時の内容を解析しておいて実行時に分岐
;    (define (analyze-if exp)
;      (let ((pproc (analyze (if-predicate exp)))
;            (cproc (analyze (if-consequent exp)))
;            (aproc (analyze (if-alternative exp))))
;        (lambda (env)
;          (if (true? (pproc env))
;            (cproc env)
;            (aproc env)))))
;
;    (define (analyze-lambda exp)
;      (let ((vars (lambda-parameters exp))
;            (bproc (analyze-sequence (lambda-body exp))))
;        (lambda (env) (make-procedure vars bproc env))))
;
;    ;; expではなくexps.
;    ;; そもそもlambdaは2個の式を実行できるようになってるの?
;    (define (analyze-sequence exps)
;      (define (sequentially proc1 proc2)
;        (lambda (env) (proc1 env) (proc2 env)))
;      (define (loop first-proc rest-procs)
;        (if (null? rest-procs)
;          first-proc
;          (loop (sequentially first-proc (car rest-procs))
;                (cdr rest-procs))))
;      (let ((procs (map analyze exps)))
;        (if (null? procs)
;          (error "Empty sequence -- ANALYZE"))
;        (loop (car procs) (cdr procs))))
;
;    ;; analyze-applicationは初期applyに似ているが解析を行わない.
;    (define (analyze-application exp)
;      (let ((pproc (analyze (operator exp)))
;            (aprocs (map analyze (operands exp))))
;        (lambda (env)
;          (execute-application (pproc env)
;                               (map (lambda (aproc) (aproc env))
;                                    aprocs)))))
;
;    (define (execute-application proc args)
;      (cond ((primitive-procedure? proc)
;             (apply-primitive-procedure proc args))
;            ((compound-procedure? proc)
;             ((procedure-body proc)
;              (extend-environment (procedure-parameters proc)
;                                  args
;                                  (procedure-environment proc))))
;            (else (error
;                    "Unknown procedure type -- EXECUTE-APPLICATION"
;                    proc))))


;; => q4.22.scm, q4.23.scm, q4.24.scm



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2. Schemeの変形 -- 遅延評価 (Variations on a Scheme -- Lazy Evaluation)
;;
;; 正規順序 <=> 作用的順序
;; 正規順序(normal order)の言語は"遅延評価"機能を持つ.
;;
;;  一番シンプルな遅延評価は, たとえばこう.
;     (define (try a b)
;       (if (= a 0) 1 b))
;     (try 0 (/ 1 0))
;; 遅延評価していれば"0で割る"エラーに出会うことなく1を返して終了する.
;; 一方, 作用的順序は手続きを作用させる前にすべて引数を評価する(ため, エラーに出会う)
;;
;; ちなみにGaucheは遅延評価している.
;     gosh> (define (try a b)
;       (if (= a 0) 1 b))
;     gosh> (try 0 (/ 1 0))
;     1

;; 遅延評価の実用的な例は"unless". 単なるifの入れ替え.
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;; 使い方
;    (unless (= b 0)
;            (/ a b)
;            (begin (display "exception: returning 0")
;                   0))

;; non-strict: 引数が評価される前に手続きの本体に入る
;; strict:     手続きの本体に入る前に引数が評価される
;;
;; 純粋作用的順序の言語 -> すべての手続きがすべての引数でstrict
;; 純粋正規順序の言語   -> すべての合成手続きがすべての引数でnon-strict

;; => q4.25.scm, q4.26.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2.2. 遅延評価の解釈系
;;   遅延引数はthunk(サンク)というオブジェクトに変換する.
;;   thunkは必要とされた時, 作用の時に評価されていたかのように引数の値を生じる.

;; (1). evalを書き換える.  => line.81-
;; (2). actual-value
(define (actual-value exp env)
  (force-it (eval exp env)))

;; (3). applyを書き換える. => line.46-
;; (4). list-of-arg-values, list-of-delayed-args. (apply中で使われる)
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
          (list-of-arg-values (rest-operands exps)
                              env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

;; (5). eval-ifを書き換える

;; (1)-(5)までやればいちおうtryが動く(この使い方だとforce-itはまだ出てこない).
; gosh> (define (try a b)
;         (if (= a 0) 1 b))
; gosh> (try 0 (/ 1 0))
; 1


;;; thunk ;;;
;; これはmemo化されていないforce-it version.1である.
; (define (force-it obj)
;   (if (thunk? obj)
;     (actual-value (thunk-exp obj) (thunk-env obj))
;     obj))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; delayしてthunkを作るのは, なんという事はない単なるtag付きのlistである.
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (delay-it exp env)
  (list 'thunk exp env))

;;; メモ化 ;;;
;; アホみたいにまんまのtagをつける. tagが thunk -> evaluated-thunkに変わる.
;; evaluated-thunk tagがついてたら評価済だからもっかいforceする必要はないということ.
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;; memo化ありのversion.2
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk) ;; tagのつけかえ
           (set-car! (cdr obj) result)     ;; 本体expをactual-value結果で置き換える
           (set-cdr! (cdr obj) '())        ;; 不要なenvをカラにする(メモリ節約)
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2.3. 遅延評価リストとしてのストリーム
;;   遅延評価において, consをnon-strictに定義すればストリームはリストそのものになる.
;;
;;   cons, car, cdrをprimitive-procedureの座から引き摺り下ろせ！
;;   => どうせdriver-loopに入れないと動かないし,
;;      primitive-procedureのcons,car,cdrが上書きされるなら同じファイルでも良いか.

;; 注意: 以下はdriver-loopを走らせてからdefすること.めどい

; (driver-loop)

;    (define (cons x y)
;      (lambda (m) (m x y)))
;
;    (define (car z)
;      (z (lambda (p q) p)))
;
;    (define (cdr z)
;      (z (lambda (p q) q)))
;
;    ;; sample procs
;
;    (define (list-ref items n)
;      (if (= n 0)
;        (car items)
;        (list-ref (cdr items) (- n 1))))
;
;    (define (map proc items)
;      (if (null? items)
;        '()
;        (cons (proc (car items))
;              (map proc (cdr items)))))
;
;    (define (scale-list items factor)
;      (map (lambda (x) (* x factor))
;           items))
;
;    (define (add-lists list1 list2)
;      (cond ((null? list1) list2)
;            ((null? list2) list1)
;            (else (cons (+ (car list1) (car list2))
;                        (add-lists (cdr list1) (cdr list2))))))
;
;    (define ones (cons 1 ones))
;    (define integers (cons 1 (add-lists ones integers)))
;
;    ;; これらの遅延リストは3章のストリームより遅延度が高い. carも遅延されてる.
;    ;; メリット: 3.5.4. でdelayかけまくっていたところが, リストがすべて遅延することによりすっきりする.
;
;    (define (integral integrand initial-value dt)
;      (define int
;        (cons initial-value
;              (add-lists (scale-list integrand dt)
;                         int)))
;      int)
;
;    (define (solve f y0 dt)
;      (define y (integral dy y0 dt))
;      (define dy (map f y))
;      y)
