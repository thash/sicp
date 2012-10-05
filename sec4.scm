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
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1. 超循環評価器(The Metacircular Evaluator)
;;
;;  > 合成手続きを一組の引数に作用させるには, 手続き本体を新しい環境で評価する.
;;  > この環境を構成するには, 手続きオブジェクトの環境部分を, 手続きの各パラメタが, 手続きを作用させる引数に束縛されるフレームで拡張する.
;;
;;  Evaluatorの実装は, 評価される式の構文(syntax)を定義する手続きに依存する. Evaluatorを実装する手続きと名前が被らないように,
;;  代入をset!ではなくassignment?, assignment-variableなどで操作するようにするなど気をつける.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.1. 評価器の中核
;;
;;  評価プロセスはevalとapplyの間の相互作用として記述できる.


;;  eval
;;    evalは引数として"式"と"環境"をとる. 式を分類して評価を振り分ける.
;;    evalの中身は式に応じた場合分けの形を取る.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


;; apply
;;   applyは引数として"手続き"と, "手続きを作用させる引数のリスト"を取る.

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

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))


(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    '#f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


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


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

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
;;


