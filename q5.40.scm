(load "./sec5.4-The-Explicit-Control-Evaluator") ;; => からの (start eceval)
(load "./sec5.5-Compilation")

;; 翻訳時環境を引数にとるcompile*を別途定義.
(define (compile* exp target linkage ct-env)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp)          (compile-quoted          exp target linkage))
        ((variable? exp)        (compile-variable        exp target linkage))
        ((assignment? exp)      (compile-assignment      exp target linkage ct-env))
        ((definition? exp)      (compile-definition      exp target linkage ct-env))
        ((if? exp)              (compile-if              exp target linkage ct-env))
        ((lambda? exp)          (compile-lambda          exp target linkage ct-env))
        ((begin? exp)           (compile-sequence (begin-actions exp) target linkage ct-env))
        ((cond? exp)            (compile* (cond->if exp) target linkage ct-env))
        ((application? exp)     (compile-application     exp target linkage ct-env))
        (else
          (error "Unknown expression type -- compile*" exp))))

;; 以下の手続きは変わらないのでskip
;; (define (make-instruction-sequence needs modifies statements)
;; (define (empty-instruction-sequence)
;; (define (compile-linkage linkage)
;; (define (end-with-linkage linkage instruction-sequence)
;; (define (compile-self-evaluating exp target linkage)
;; (define (compile-quoted exp target linkage)
;; (define (compile-variable exp target linkage) ;; <= ?

;; 内部でcompileを読んでいるcompile-assignment, compile-definition, compile-if, compile-lambda,
;; compile-sequence, compile-application を, ct-envを渡してcompile*を使うよう修正.
(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code ; [new]
          (compile* (assignment-value exp) 'val 'next ct-env))) ; **
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op set-variable-value!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))


(define (compile-definition exp target linkage ct-env)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile* (definition-value exp) 'val 'next ct-env))) ; **
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-if exp target linkage ct-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile* (if-predicate exp) 'val 'next ct-env)) ; **
            (c-code (compile* (if-consequent exp) target consequent-linkage ct-env)) ; **
            (a-code (compile* (if-alternative exp) target linkage ct-env))) ; **
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

(define (compile-sequence seq target linkage ct-env)
  (if (last-exp? seq)
    (compile* (first-exp seq) target linkage ct-env) ; **
    (preserving '(env continue)
                (compile* (first-exp seq) target 'next ct-env) ; **
                (compile-sequence (rest-exps seq) target linkage))))

;; compile-lambda自体はまだct-envを渡すだけ.
(define (compile-lambda exp target linkage ct-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequence
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
                            (make-instruction-sequence '(env) (list target)
                                                       `((assign ,target
                                                                 (op make-compiled-procedure)
                                                                 (label ,proc-entry)
                                                                 (reg env)))))
          (compile-lambda-body exp proc-entry ct-env)) ; **
        after-lambda))))

;; [重要] ただct-env渡すだけではないuniqueな変更点はココのみ.
(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp))) ; formalsってのは束縛した変数
    (append-instruction-sequence
      (make-instruction-sequence '(env proc argl) '(env)
                                 `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return (cons formals ct-env))))) ; *** ct-envに変数を追加！

(define (compile-application exp target linkage ct-env)
  (let ((proc-code (compile* (operator exp) 'proc 'next ct-env)) ; **
        (operand-codes
          (map (lambda (operand) (compile* operand 'val 'next ct-env)) ; **
               (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

