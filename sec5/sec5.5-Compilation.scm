;;;   5.5 翻訳系(Compilation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./sec4/sec4.1-The-Metacircular-Evaluator.scm")

;; 高レベル言語とレジスタ計算機の間を橋渡しする方法はふたつある.
;;
;; 1. 解釈(interpretation, インタープリタ言語) -- 5.4の積極評価器計算機で使った戦略.
;; 2. 翻訳(compilation, コンパイラ言語)戦略
;;
;; 5.5では2のコンパイラ言語として使えるようにする.
;; 与えられたソースコードを機械語で書いた等価な目的プログラム(object program)に変換する.
;; 具体的には, Schemeコードをレジスタ計算機の命令列に変換するようにする.

;; 解釈(インタプリタ)系が式を評価するとき, それはあらゆる偶然に備えなければならない.
;; 必要となるかもしれないすべてのレジスタを退避しておく必要が有る.
;; 他方翻訳(コンパイラ)系は, 処理しようとしている式の構造を調べ, 不要なスタック演算を避ける事が出来る.
;; また, lookup-variable-value探索をせずそのフレームへ直接アクセスする最適化も可能になる(sec5.5.6).


;;;     5.5.1 翻訳系の構造
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; トップレベルの振り分け処理compile
(define (compile exp target linkage)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp)          (compile-quoted          exp target linkage))
        ((variable? exp)        (compile-variable        exp target linkage))
        ((assignment? exp)      (compile-assignment      exp target linkage))
        ((definition? exp)      (compile-definition      exp target linkage))
        ((if? exp)              (compile-if              exp target linkage))
        ((lambda? exp)          (compile-lambda          exp target linkage))
        ((begin? exp)           (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp)            (compile (cond->if exp) target linkage))
        ((application? exp)     (compile-application     exp target linkage))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

;; target = 翻訳したコードが式の値を返すレジスタを指定する
;; linkage = 接続記述. 式の翻訳の結果コードが実行を終了した時どこへ行くかを記述する
;;           next, return, gotoのいずれか.

;; self-evaluatingな5という式をtarget val, linkage nextで翻訳すると...
;    5 -(翻訳)-> (assign val (const 5))
;; 一方 linkage return ならばcontinueを使って手続き呼び出しから戻る.
;    (assign val (const 5))
;    (goto (reg continue))


;;; 命令列とスタックの利用
;;
;; まず単純に命令列を<seq1><seq2>とつなげるには
;;     (append-instruction-sequence <seq1> <seq2>)
;;     ;; => 実装はp.352(sec5.5.4)
;; とすればいい. レジスタをstackに退避しつつ, という時はpreservingを使うのだが,
;;     (preserving (list <reg1> <reg2>) <seq1> <seq2>)
;; seq1,2がreg1,2を"どう使うか"により4通りの命令列を作ることになる.
;;   1. <seq1><seq2>
;;   2. (save <reg1>) <seq1> (restore <reg1>) <seq2>
;;   2. (save <reg2>) <seq1> (restore <reg2>) <seq2>
;;   2. (save <reg2>) (save <reg1>) <seq1> (restore <reg1>) (restore <reg2>) <seq2>
;;
;; このへんのsave/restore手続きはpreservingの中に隠蔽してしまおうという方針.
;;
;;
;; 命令列は3要素からなると考える.
;;   1. 列の命令を実行する前に初期化しなければならないレジスタの集合
;;   2. 列の命令が値を修正するレジスタの集合
;;   3. 列の実際の命令(文:statements)

;; 命令列の構成子はこうなる.
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
;; 空の命令列を作るときもある
(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;;     5.5.2 式の翻訳
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
          (make-instruction-sequence '() '()
                                     `((goto (label ,linkage))))))) ; quasiquoteの利用


;; 多用することになるend-with-linkageを定義
;; preserving '(continue) なのでcontinueを保存する必要がある時は保存する.
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

;; 単純式の翻訳
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '(env) (list target)
                                               `((assign ,target
                                                         (op lookup-variable-value)
                                                         (const ,exp)
                                                         (reg env))))))

;;; 代入/定義命令
;; envとvalを必要とし, targetを修正する.
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code ; [new]
          (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op set-variable-value!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

;;; 条件式の翻訳
;   (test (op false?) (reg val))
;   (branch (label false-branch))
; true-branch
;   <compilation of consequent  with given target, linkage, after-if>
; false-branch
;   <compilation of alternative with given target, linkage>
; after-if
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp) target consequent-linkage))
            (a-code (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequence
                      (make-instruction-sequence '(val) '()
                                                 `((test (op false?) (reg val))
                                                   (branch (label ,f-branch))))
                      (parallel-instruction-sequence
                        (append-instruction-sequence t-branch c-code)
                        (append-instruction-sequence f-branch a-code))
                      after-if))))))

;; 脚注
;; もちろん実用の際は複数のif文があるので混ざってしまわないようにラベル名を連番にする.
;; Query languageで変数に番号をつけた時のようにmake-labelを実装しよう.
(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

;; 並び(sequence)の翻訳
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage)
    (preserving '(env continue)
                (compile (first-exp seq) target 'next)
                (compile-sequence (rest-exps seq) target linkage))))

;; lambda式の翻訳
(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequence
        (tack-on-instruction-sequence ; [new] compile-lambdaでのみ利用.
          (end-with-linkage lambda-linkage
                            (make-instruction-sequence '(env) (list target)
                                                       `((assign ,target
                                                                 (op make-compiled-procedure) ; [new]
                                                                 (label ,proc-entry)
                                                                 (reg env)))))
          (compile-lambda-body exp proc-entry)) ; [new]
        after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequence
      (make-instruction-sequence '(env proc argl) '(env)
                                 `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return))))

;; 脚注よりlambda翻訳の補助手続き
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))


;;;     5.5.3 組み合わせの翻訳
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 翻訳処理の実質は, 手続き作用の翻訳である.
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
               (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes) ; [new]
                            (compile-procedure-call target linkage))))) ; [new]

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
                                 `((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequence
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           `((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving '(env)
                      code-to-get-last-arg
                      (code-to-get-rest-args ; [new] 紛らわしい.
                        (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 `((assign argl
                                                           (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

;;; 手続きの作用
;; 組み合わせの要素を翻訳したあと, 翻訳コードはprocにある手続きをarglにある引数に作用させなければならない.

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
            (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequence
        (make-instruction-sequence '(proc) '()
                                   `((test (op primitive-procedure?) (reg proc))
                                     (branch (label ,primitive-branch))))
        (parallel-instruction-sequence
          (append-instruction-sequence
            compiled-branch
            (compile-proc-appl target compiled-linkage)) ; [new]
          (append-instruction-sequence
            primitive-branch
            (end-with-linkage linkage
                              (make-instruction-sequence '(proc argl)
                                                         (list target)
                                                         `((assign ,target
                                                                   (op apply-primitive-procedure)
                                                                   (reg proc)
                                                                   (reg argl)))))))
        after-call))))


;;; 翻訳した手続きの作用
;; "翻訳した手続き(a compiled procedure)"はcompile-lambdaにより生成されたもの.
;; ... と言うもんだからproc-returnはlambdaを翻訳した時に出るのかと思ったけど,
;; lambdaに限らず手続き呼び出しの時に出てくる.
;; (lambda (x) (* x x))ではprocを作るだけなので出てこないけど
;; ((lambda (x) (* x x)) 2)とかやるとproc-returnが現れる.

;; > compile-proc-appl は, 呼び出しの標的がvalかどうか, 接続がreturnかどうかによる4つの場合を考慮して,
;; > 上の手続き作用のコードを生成する.
;; すべてのregisterが修正されうるのでmodifiesに全部入っている(all-regs)ことに注意.
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return))) ; case1
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                                  (reg proc))
                                      (goto (reg val)))))
        ;; case2
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                    (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ;; case3
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    `((assign val (op compiled-procedure-entry)
                                                  (reg proc))
                                      (goto (reg val)))))
        ;; case4
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; 脚注より
(define all-regs '(env proc val argl continue))


;;;     5.5.4 命令列の組み合わせ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


;; 何度も使われてきたappend-instruction-sequenceをようやく実装
(define (append-instruction-sequence . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences (car seqs)
                          (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

;; 集合の表現
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;; 命令列組合わせ手続きpreserving
(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequence seq1 seq2)
    (let ((first-reg (car regs)))
      (display `("preserving: " ,first-reg))
      (if (and (needs-register? seq2 first-reg)
               (modifies-register? seq1 first-reg))
        (begin (print "  -> true")
               (preserving (cdr regs)
                           (make-instruction-sequence
                             (list-union (list first-reg)
                                         (registers-needed seq1))
                             (list-difference (registers-modified seq1)
                                              (list first-reg))
                             (append `((save ,first-reg))
                                     (statements seq1)
                                     `((restore ,first-reg))))
                           seq2))
        (begin (print "  -> false")
               (preserving (cdr regs) seq1 seq2))))))

;; another 列組合わせ手続き
;; seqにはいつものend-with-linkage結果が, body-seqにはcompile-lambda-bodyの結果が入る.
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))
;; lambda-bodyは順序の大事な他の翻訳と違って, 本体がどこにあってもいい.
;; label付けつつ適当にくっつけるときの手続き, らしい.
;; ちょっと効率良くするためにこれ使うのかな?

;; testのあとの2つの選択肢を連結するのがparallelなんちゃら
;; どちらかしか通らないコードを作る.
(define (parallel-instruction-sequence seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))


;;;     5.5.6 文面アドレス Lexical Addressing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 翻訳系を最適化しようとしたとき, ありがちなのは変数探索の最適化.
;; lookup-variable-valueは常にframeの最初から探索するから,
;; 深いところの変数を何度も参照すると無駄な比較が大量に発生する. これを改良しよう.
;; letはlambdaとみなせるし, defineは使わないと仮定すれば変数はlambdaだけで束縛される.
;; (defineを使う場合の話はq5.43.scmにて)

;; そこで文面アドレス(lexical address)を導入する.
;; > a lexical address that consists of two numbers:
;; >   a frame number, which specifies how many frames to pass over, and
;; >   a displacement number, which specifies how many variables to pass over in that frame.
;; (1 2) -> 1個のframeを読み飛ばし, 2個の変数を読み飛ばしたところにある変数を指す.
;; 文面アドレスは相対的なものなのでどこから参照するかによって表現は違ってくる

;; 文面アドレス実装法の1つは翻訳時環境(compile-time environment)というデータ構造を維持すること.
;; ある変数アクセス演算を実行する時に, どの変数が実行時環境のどのフレームのどの場所にあるかを記憶.
;;   * 翻訳時環境は"それぞれが変数のリストを持つフレーム"のリストである.
;;   * 翻訳時環境には変数名だけ保持, 値は計算しない.
;;   * 翻訳時環境はcompileの追加引数になる

;;; 問題
;; q5.39.scm で実質的な実装を行う.
;;  その後も問題がほぼ本文扱いなので, 各問題を独立させずうまく引き継いでいく必要がある.
;; q5.40.scm compileに引数を追加. ct-envというのを翻訳時環境.
;;   compile-lambda-bodyの変化に着目. (cons formals ct-env) を渡す.
;;   どこで新しくframeを作るか, を自覚的に実装できるレイヤーを触っている.
;; q5.41.scm 引数として変数と翻訳時環境ct-envを取り, その環境に対する変数の文面アドレスを返す手続きfind-variableを書け.
;;   見つかったらmake-lexical addressを使って文面アドレスを返す.
;; q5.42.scm find-variableを使い, compile-assignmentとcompile-variableを書きなおして文面アドレスを返すように書き換える.
;;   出力されたmachine codeを見ると(op lexical-address-lookup) (const (0 1))などを生成するはず.
;; q5.43.scm lambdaの中でのdefineは真のdefineと解釈するのではなく(let (u `*unassigned))などとしろ, という議論が以前あった(根拠とか要復習)
;;   compile-lambda-bodyを書き換えてunassigned


;;;     5.5.7 翻訳したコードと評価器のインターフェイス
;;;           Interfacing Compiled Code to the Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; まず翻訳機が翻訳した手続きを使えるようにapply-dispatchまわりのmachine codeを修正

;; 解釈と翻訳
;; 解釈系はstepが抽象化できてbug取りもすぐできる
;; 翻訳系は機械語で実装されているので実行が速い

;;; 問題
;; q5.45.scm 翻訳版, 解釈版でのpush回数, max depthをNの式で表せ.
;;   解釈/翻訳の比率が収束するのでそれを計算してみよ.
;; q5.46.scm
;;   push回数は線形に増えていないっぽいと.
;; q5.47.scm 今の実装では合成手続きを呼べない. これを呼べるようにせよと.
;;   compile-procedure-callの修正.
;;   いままではprimitiveかcompiledかしか扱えなかったけど, compundも判定するようにするよと.
;; q5.48.scm compile-and-goは1個しか翻訳できず使い勝手が悪い. 繰り返し使えるcompile-and-runを作れ.
;;   compile-and-runの中でglobal環境の初期化をやらないようにしている.
;;   駆動ループの中でもっかいcompileするようなコードを動かす.

