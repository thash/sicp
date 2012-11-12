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

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

;;  eval
;;    evalは引数として"式"と"環境"をとる. 式を分類して評価を振り分ける.
;;    evalの中身は式に応じた場合分けの形を取る.

;; 注意: 4.1.7. にてanalyze版に上書きされる
;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((let? exp) (eval (let->combination exp) env))
;        ((lambda? exp)
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env))
;        ((begin? exp)
;         (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((application? exp)
;         (apply (eval (operator exp) env)
;                (list-of-values (operands exp) env)))
;        (else
;          (error "Unknown expression type -- EVAL" exp))))



;; list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;; eval-if
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
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
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline)
          (display ";;; Starting a new problem ")
          (ambeval input
                   the-global-environment
                   ;; ambeval 成功
                   (lambda (val next-alternative)
                     (annouce-output output-prompt)
                     (user-print val)
                     (internal-loop next-alternative))
                   ;; ambeval 失敗
                   (lambda ()
                     (annouce-output
                       ";;; There are no more values of")
                     (user-print input)
                     (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))


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
;; "内部で定義した名前が真に同時有効範囲を持つように定義を扱う" -- 評価する前に未代入変数を定義する.
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

;; > 内部定義を掃き出すもう一つの戦略は, q4.18.scm に示す. これは「定義された変数の値は, いずれの変数の値も使わずに評価するように強制する」ことである.

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
(define (eval exp env)
  (analyze exp) env)

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

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
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;; 変数探索は実行フェイズに行わなければならない.
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; assignmentとdefinitionは1度だけ解析すれば良い.
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

;; if は解析時に条件文, trueの時, elseの時の内容を解析しておいて実行時に分岐
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; pred-valueを得るための述語評価の成功継続
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             ;; 述語評価の失敗継続
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;; expではなくexps.
;; そもそもlambdaは2個の式を実行できるようになってるの?
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; aを呼び出す時の成功継続
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; bを呼び出す時の失敗継続
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


;; analyze-applicationは初期applyに似ているが解析を行わない.
(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else (error
                "Unknown procedure type -- EXECUTE-APPLICATION"
                proc))))


;; => q4.22.scm, q4.23.scm, q4.24.scm



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.3. Schemeの変形 -- 非決定性計算(Nondeterministic Computing)
;;   そそる響き.

;; 例. ２つの整数のリストがあり, そこから1個ずつとった数の和が素数になるかどうか調べる.
;;     フィルタする方法を3章で学んだ.
;;     非決定性計算は, 要求されることをプログラムに書き下す形で実現する.

;    (define (prime-sum-pair list1 list2)
;      (let ((a (an-element-of list1))
;            (b (an-element-of list2)))
;        (require (prime? (+ a b)))
;        (list a b)))

;; 動き方イメージ (prime?は実装済と想定)
;; (prime-sum-pair '(1 3 5 8) '(20 35 110))
;;   => (3 20)

;; ポイント: 非決定性計算においては "式はひとつ以上の可能な値を持ち得る".
;;           .oO (単純な置き換えモデルがもはや通用しないか...)

; > 非決定性評価とストリーム処理で惹き起こされる時間の異なる姿を対照させることは有用である.
; > ストリーム処理は, 可能性ある解答のストリームが集められる時と, 実際のストリーム要求が作り出される時を切り離すのに遅延評価を使う. 評価器は, 可能な答のすべては無時間の並びとしてわれわれの前に存在するという幻想を作る.
; > 非決定性計算では, 式は一連の選択で決められる, 可能な世界の集合の探索を表す. 可能な世界の中には, 行き止まりのものもあるが, 残りは有用な値を持つ. 非決定性プログラムの評価器は, 時は分岐し, プログラムは異なる可能な実行履歴を持つという幻想を作る.


;;; 特殊形式 amb と探索 ;;;
;; ambの使い方イメージ (sec4.3.3.まで実装しないと評価器が動かない)
;; 引数付きのambは引数(=候補)の中からひとつの値を"ambivalentに"返す.
;    (list (amb 1 2 3) (amb 'a 'b))
;      => (1 a), or (1 b), or (2 a), or (2 b), or (3 a), or (3 b)

;; 引数なしのambは"受け入れられる値がない" => たどり着いてしまうと失敗になる式, と考える.
;; これを使えばrequireも実装できる.
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; ambは無限の表現も可能である. まぁ3章の無限streamと同じだね
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;;; ambのdriver-loop ;;;
;; try-againという記号でコントロールする.

;; => q4.35.scm, q4.36.scm, q4.37.scm

;        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        ;; 4.3.2. 非決定性プログラムの例
;
;        ;; amb使えば論理パズル解けるよー (ええからはよ実装せえや)
;        ;; 問題を論理的に書き下せる時点で解けとるようなもんやん
;        (define (multiple-dwelling)
;          (let ((baker (amb 1 2 3 4 5))
;                (cooper (amb 1 2 3 4 5))
;                (fletcher (amb 1 2 3 4 5))
;                (miller (amb 1 2 3 4 5))
;                (smith (amb 1 2 3 4 5)))
;            (require
;              (distinct? (list baker cooper fletcher miller smith)))
;            (require (not (= baker 5)))
;            (require (not (= cooper 5)))
;            (require (not (= fletcher 5)))
;            (require (not (= fletcher 1)))
;            (require (> miller cooper))
;            (require (not (= (abs (- smith fletcher)) 1)))
;            (require (not (= (abs (- fletcher cooper)) 1)))
;            (list (list 'baker baker)
;                  (list 'cooper cooper)
;                  (list 'fletcher fletcher)
;                  (list 'miller miller)
;                  (list 'smith smith))))
;
;        ;; => q4.38.scm, q4.39.scm, q4.40.scm, q4.41.scm, q4.42.scm, q4.43.scm, q4.44.scm
;
;
;        ;;; 自然言語の構文解析 ;;;
;        ;; (ええからはよ実装せえや)
;
;        ;; 品詞に分解
;        (define nouns '(noun student professor cat class))
;        (define verbs '(verbs studies lectures eats sleeps))
;        (define articles '(articles the a))
;
;        ;; 文法.
;        ;; The cat eats を構文解析すると
;        (sentence (noun-phrase (article the) (noun cat))
;                  (verb eats))
;
;        ;; 道具箱
;        (define (parse-sentence)
;          (list 'sentence
;                (parse-noun-phrase)
;                (parse-word verbs)))
;
;        (define (parse-noun-phrase)
;          (list 'noun-phrase
;                (parse-word articles)
;                (parse-word nouns)))
;
;        (define (parse-word word-list)
;          (require (not (null? *unparsed*)))
;          (require (memq (car *unparsed*) (cdr word-list)))
;          (let ((found-word (car *unparsed*)))
;            (set! *unparsed* (cdr *unparsed*))
;            (list (car word-list) found-word)))
;
;
;        ;; こうやって使う
;        (define *unparsed* '())
;
;        (define (parse input)
;          (set! *unparsed* input)
;          (let ((sent (parse-sentence)))
;            (require (null? *unparsed*))
;            sent))
;
;        ;; 探索とバックトラックは複雑な文法を扱うとき本当に有効である(ええからはよ実装せえや)
;        (define prepositions '(prep for to in by with))
;
;        (define (parse-prepositional-phrase)
;          (list 'prep-phrase
;                (parse-word prepositions)
;                (parse-noun-phrase)))
;
;        (define (parse-sentence)
;          (list 'sentence
;                (parse-noun-phrase)
;                (parse-verb-phrase)))
;
;        ;; ようやくamb出てきた
;        (define (parse-verb-phrase)
;          (define (maybe-extend verb-phrase)
;            (amb verb-phrase
;                 (maybe-extend (list 'verb-phrase
;                                     verb-phrase
;                                     (parse-prepositional-phrase)))))
;          (maybe-extend (parse-word verbs)))
;
;        ;; ついでに名刺句の定義を改善する
;        (define (parse-simple-noun-phrase)
;          (list 'simple-noun-phrase
;                (parse-word articles)
;                (parse-word nouns)))
;
;        (define (parse-noun-phrase)
;          (define (maybe-extend noun-phrase)
;            (amb noun-phrase
;                 (maybe-extend (list 'noun-phrase
;                                     noun-phrase
;                                     (parse-prepositional-phrase)))))
;          (maybe-extend (parse-simple-noun-phrase)))
;
;        ;; ここまでやると, 以下のような複雑な文を解析できる(うれしげに言うとらんと実装せえっちゅうに)
;        (parse '(the student with the cat sleeps in the class))
;        ;; =>
;        (sentence
;          (noun-phrase
;            (simple-noun-phrase (articles the) (noun student))
;            (prep-phrase (prep with)
;                         (simple-noun-phrase
;                           (article the) (noun cat))))
;          (verb-phrase
;            (verb sleeps)
;            (prep-phrase (prep in)
;                         (simple-noun-phrase
;                           (article the) (noun class)))))
;
;        ;; 別の例
;        (parse '(the professor lectures to the student with the cat))
;        ;; =>
;        (sentence
;          (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;            (verb-phrase
;              (verb lectures)
;              (prep-phrase (prep to)
;                           (simple-noun-phrase
;                             (article the) (noun student))))
;            (prep-phrase (prep with)
;                         (simple-noun-phrase
;                           (article the) (noun cat)))))
;        ;; => 再実行
;        (sentence
;          (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;            (verb lectures)
;            (prep-phrase (prep to)
;                         (noun-phrase
;                           (simple-noun-phrase
;                             (article the) (noun student))
;                           (prep-phrase (prep with)
;                                        (simple-noun-phrase
;                                          (article the) (noun cat)))))))
;
;        ;; => q4.45.scm, q4.46.scm, q4.47.scm, q4.48.scm, q4.49.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.3.3. amb評価器の実装
;;   通常のScheme式を評価すると 値が返る, 停止しない, エラーになる のいずれかとなる.
;;   非決定性Schemeでは, 式の評価はこれに加え "袋小路の発見" があり得る.
;;   袋小路を発見すると直前の選択点へバックトラックしなければならない. コイツのせいで複雑である.

;; ambは環境とふたつの"継続"手続き, 計3つの引数をとる.
;; 2つの継続とは成功継続(success continuation), 失敗継続(failure continuation)である.
;; amb評価器の複雑さのほとんどは実行手続きが互いに呼び出す時の, 継続を引き渡す機構に由来する.

;; amb ... 評価器に組み込む.
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; > またanalyzeの振り分け部に, この特殊形式を認識し, 適切な実行手続きを生成する節を追加しなければならない.
;; .oO (analyzeはなかったことになったと思ってた)

;; トップレベルの手続き ambeval
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;; 実行手続きの一般形はこんな感じ
;    (lambda (env succeed fail)
;      (lambda (value fail) ...) ;; succeedのかたち
;      (lambda () ...) ;; failのかたち
;      )
;
;; 例:
;    (ambeval <exp>
;             the-global-environment
;             (lambda (value fail) value)
;             (lambda () 'failed))


(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
                  ;;このaprocsの成功継続
                  (lambda (arg fail2)
                    (get-args (cdr aprocs)
                              env
                              ;;get-argsの再帰呼び出しの成功継続
                              (lambda (args fail3)
                                (succeed (cons arg args)
                                         fail3))
                              fail2))
                  fail)))


(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))

;; => q4.52.scm, q4.53.scm, q4.54.scm


;; test
; ;; analyze版でletを使えるようにしないといけない
; (driver-loop)
; (define count 0)
; (let ((x (an-element-of '(a b c)))
;       (y (an-element-of '(a b c))))
;   (set! count (+ count 1))
;   (require (not (eq? x y)))
;   (list x y count))


