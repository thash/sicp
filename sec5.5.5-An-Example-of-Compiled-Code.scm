(load "./sec5.5-Compilation")

(compile
  '(define (factorial n)
    (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
  'val
  'next)

;; これを出力するとこうなった. 命令列を単位毎に改行している

((env)
 (val)
 (
  ;; 手続きを構成し手続き本体のコードを飛び越す
  (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1 ; factorialの呼び出しはここから始まる
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; 手続き本体の開始
  (save continue)
  (save env)

  ;; (= n 1) の計算
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  compiled-branch7
  (assign continue (label after-call8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call8 ; valには(= n 1)の結果がある
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
  true-branch3 ; 1を返す
  (assign val (const 1))
  (goto (reg continue))
  ;; (* (factorial (- n 1)) n)を計算し返す
  false-branch4
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc) ; [*] 手続きを退避
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl) ; * の部分引数リストを退避

  ;; * のもうひとつの引数(factorial (- n 1))の計算
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc) ; factorial手続きを退避

  ;; factorialの引数(- n 1)の計算
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
  compiled-branch10
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call11 ; valには(- n 1) の結果がある
  (assign argl (op list) (reg val))
  (restore proc) ; factorialを回復

  ;; factorialを作用させる
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
  compiled-branch13
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call14 ; valには(factorial (- n 1))の結果がある
  (restore argl) ; * の部分引数リストを回復
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ; * を回復
  (restore continue)
  ;; * を作用させ値を返す
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch15))
  compiled-branch16
  ;; 合成手続きは末尾再帰的に呼び出されることに注意
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call17
  after-if5
  after-lambda2
  ;; 手続きを変数factorialに代入
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
  ))

