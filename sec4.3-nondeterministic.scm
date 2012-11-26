;; non-deterministicに書き換えた評価器本体 {{{1

(define true #t)
(define false #f)

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

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.2. 式の表現

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
    'false))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.3. 評価器のデータ構造

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

;; これは本文の進行に応じて増やしていく.
;; ambはprimitive-proceduresではない. 特殊形式として実装する.
;; requireはprimitive-proceduresではない. driver-loop起動後にdefineする.
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list 'and and)
        (list 'or or)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '>= >=)
        (list '< <)
        (list '<= <=)
        (list '= =)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
        (list 'even? even?)
        (list 'display display)
        (list 'newline newline)
        (list 'map map) ;; for q4.43.scm
        (list 'set! set!) ;; for parse-word
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
          (newline)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.7. 構文解析を実行から分離する
;;

;; 注意: sec4.scm全体をloadした場合, 冒頭のevalを上書きする.
;; 非決定性Schemeではこのevalは使われない(代わりにambevalを使う).
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

;; まず考える単純なパターンでは, succeedしか有り得ない(常に成功する).
;; ただし失敗継続はそのまま引き回す.
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

;; assignmentとdefinitionは1度だけ解析すれば良い(vproc).
;; *A*: vprocはset!で始まるclosure. 成功継続が(lambda (val fail2))のブロック.
;; *1*: 成功継続. ここではset!の作用を起こした後に(succeed exp fail)形を返す.
;; *B*: 実際に代入する前にold-valueに保存
;; *2*: 将来の失敗継続. 代入し直してold-valueに戻す処理.
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env ; *A*
             (lambda (val fail2) ; *1*
               (let ((old-value ; *B*
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

(define (execute-application proc args succeed fail)
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

;; }}}1

;; 4.3. Schemeの変形 -- 非決定性計算(Nondeterministic Computing) {{{1
;;   評価器に児童探索の機能を組み込む.

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
;;           (単純な置き換えモデルが通用しない)

; > 非決定性評価とストリーム処理で惹き起こされる時間の異なる姿を対照させることは有用である.
; > ストリーム処理は, 可能性ある解答のストリームが集められる時と, 実際のストリーム要求が作り出される時を切り離すのに遅延評価を使う. 評価器は, 可能な答のすべては無時間の並びとしてわれわれの前に存在するという幻想を作る.
; > 非決定性計算では, 式は一連の選択で決められる, 可能な世界の集合の探索を表す. 可能な世界の中には, 行き止まりのものもあるが, 残りは有用な値を持つ. 非決定性プログラムの評価器は, 時は分岐し, プログラムは異なる可能な実行履歴を持つという幻想を作る.


;;; 特殊形式 amb と探索 ;;;
;; ambの使い方イメージ (sec4.3.3.まで実装しないと評価器が動かない)
;; 引数付きのambは引数(=候補)の中からひとつの値を"ambivalentに"返す.
;    (list (amb 1 2 3) (amb 'a 'b))
;      => (1 a), or (1 b), or (2 a), or (2 b), or (3 a), or (3 b)

;; 引数なしのambは"受け入れられる値がない" => たどり着いてしまうと失敗になる式, と考える.
;; これを使えばrequireも実装できる. }}}1


;;; NOTE: driver-loop起動後にrequireとan-element-ofを定義する.
; (driver-loop)

; (define (require p)
;   (if (not p) (amb)))
;
; ;; itemsがnullでないことをrequireしている.
; ;; つまり候補がnullならrequireのレイヤーで(amb)つまり失敗を返す
; (define (an-element-of items)
;   (require (not (null? items)))
;   (amb (car items) (an-element-of (cdr items))))
;
; ;; ambは無限の表現も可能である. まぁ3章の無限streamと同じだね
; (define (an-integer-starting-from n)
;   (amb n (an-integer-starting-from (+ n 1))))

;;; ambのdriver-loop ;;;
;; try-againという記号でコントロールする.

;; => q4.35.scm, q4.36.scm, q4.37.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.3.2. 非決定性プログラムの例
;; ↑は4.3.3 で評価器を実装してからじゃないと実践できないので別ファイルに切り出し.
;;     => sec4.3.2-Examples-of-Nondeterministic-Programs.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.3.3. amb評価器の実装
;;   通常のScheme式を評価すると 値が返る, 停止しない, エラーになる のいずれかとなる.
;;   非決定性Schemeでは, 式の評価はこれに加え "袋小路の発見" があり得る.
;;   袋小路を発見すると直前の選択点へバックトラックしなければならない. コイツのせいで複雑である.

;; ambは環境とふたつの"継続"手続き, 計3つの引数をとる.
;; 継続について: http://bit.ly/keizoku
;; 2つの継続とは成功継続(success continuation), 失敗継続(failure continuation)である.
;; amb評価器の複雑さのほとんどは実行手続きが互いに呼び出す時の, 継続を引き渡す機構に由来する.

;; amb ... 評価器に組み込む.
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; > またanalyzeの振り分け部に, この特殊形式を認識し, 適切な実行手続きを生成する節を追加しなければならない.
;; ((amb? exp) (analyze-amb exp))
;; をanalyzeに追加

;; トップレベルの手続き ambeval. analyze分離後のevalに代わるもの.
;    (define (eval exp env)
;      (analyze exp) env)
;; 引数に成功継続と失敗継続が加わっている.
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

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

;; 実行手続きの一般形
;    (lambda (env succeed fail)
;      (lambda (value fail) ...) ;; 成功継続
;      (lambda () ...) ;; 失敗継続
;      )
;
;; 例:
;    (ambeval <exp>
;             the-global-environment
;             (lambda (value fail) value)
;             (lambda () 'failed))


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

;(display "ここでrequire, an-element-ofをdriver-loop中にload")
;(driver-loop)

;(define (require p)
;  (if (not p) (amb)))
;
;;; an-element-of の選び方(car取って再帰し, 前から順に調べていく)は深さ優先探索になっている.
;;; ランダムに選びたくなるが, 規則的に探索する(systematically search)のがよい.
;(define (an-element-of items)
;  (require (not (null? items)))
;  (amb (car items) (an-element-of (cdr items))))

;;; 用例
;;;    (define (an-integer-starting-from n)
;;;      (amb n (an-integer-starting-from (+ n 1))))


