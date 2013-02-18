;; 手続き作用の評価では, 積極制御評価器は常に
;;   * 演算子の評価の前後でenvレジスタを退避回復し,
;;   * (最後のものを除いて)各被演算子の前後でenvを退避回復し,
;;   * 各被演算子の前後でarglを退避回復し,
;;   * 被演算子列の評価の前後でprocを退避回復
;; する. 次の組み合わせのそれぞれで, これらのsave/restoreのどれが余分であり,
;; 翻訳系のpreserving機構で除けるかを示せ.

(f 'x 'y)     ; No.1
((f) 'x 'y)   ; No.2
(f (g 'x) y)  ; No.3
(f (g 'x) 'y) ; No.4

(load "./sec5.5-Compilation")
(define (p lis)
  (if (not (null? lis))
      (begin
        (if (pair? (caar lis))
            (map (lambda (x) (if (symbol? x) (print x) (print "  " x))) (car lis))
            (print (car lis)))
        (p (cdr lis)))))

;; 関数の中で関数適用するとproc, arglistの退避が必要になる.

;; メモ
;; * 変数(const?)はlookup-variable-valueする
;; * symbolはしない. そのままvalになる

;; No.1 (f 'x 'y)
;; gosh> (p (compile '(f 'x 'y) 'val 'next))
(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x)) ; yとxを順々にarglに入れているが中間変数valしか使えないのでめんどそう
  (assign argl (op cons) (reg val) (reg argl))
  ;; proc = f がprimitive-procedureかどうかtest
  ;; (sec4.1でprimitive-procedureを明示的にリストアップしている)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch15))
compiled-branch16
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17



;; No.2 ((f) 'x 'y)
;; これは退避しなくても良い. procから直接procへ入れてるので. (特殊な最適化)
;; gosh> (p (compile '((f) 'x 'y) 'val 'next))
(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (assign argl (const ())) ; f の手続き作用に渡す引数... は, ない.
  ;; f = procがprimitive-procedureならprimitive-branchへgoto(軽くjump)してそれ以外は直進,
  ;; というのがお決まりのパターンぽい
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1))
compiled-branch2
  (assign continue (label proc-return4))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
proc-return4
  (assign proc (reg val))
  (goto (label after-call3))
primitive-branch1 ; f = proc がprimitive-procedureの時
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
after-call3
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
compiled-branch6
  (assign continue (label after-call7))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call7


;; No.3 (f (g 'x) y)
;; gosh> (p (compile '(f (g 'x) y) 'val 'next))
(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc) ; saveが出てきたのはNo.3が初. fとgの2個がprocとなり得るため.
  (assign val (op lookup-variable-value) (const y) (reg env))
  (assign argl (op list) (reg val))
  (save argl) ; proc saveするならarglも. proc作用環境が1つの世界というイメージ?
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (const x))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1))
compiled-branch2
  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch1
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call3
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch4))
compiled-branch5
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch4
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call6


;; No.4 (f (g 'x) 'y)
;; 3との違いはyがsymbolであること
;; gosh> (p (compile '(f (g 'x) 'y) 'val 'next))
(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc)
  (assign val (const y))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (const x))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1))
compiled-branch2
  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch1
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call3
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch4))
compiled-branch5
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch4
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call6


;; preservingに以下の様な感じでprintを仕込んで見たところ, true conditionに入るケースはとても少ない.
;; 4個通してみてもNo.3のprocで1回, No.4のarglとprocで2回しかtrueに入っていない.
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

