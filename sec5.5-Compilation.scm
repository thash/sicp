;;;   5.5 翻訳系(Compilation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; 一方 linkage return ならば以下の命令を生じる.
;    (assign val (const 5))
;    (goto (reg continue))


;;; 命令列とスタックの利用
;;
;; まず単純に命令列を<seq1><seq2>とつなげるには
;;     (append-instruction-sequence <seq1> <seq2>)
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
         (make-instruction-sequence '() '()
                                    (empty-instruction-sequence)))
        (else
          (make-instruction-sequence '() '()
                                     `((goto (label ,linkage))))))) ; quasiquoteの利用


;; 多用することになるend-with-linkageを定義
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

;; イマココ p.345
; compile-lambda
; compile-sequence
; compile-application
