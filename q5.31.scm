(f 'x 'y)
((f) 'x 'y)
(f (g 'x) y)
(f (g 'x) 'y)

(define (parse-compiled-code lis)
  (if (not (null? lis))
      (begin
        (if (pair? (caar lis))
            (map (lambda (x)
                         (if (symbol? x)
                             (print x)
                             (print "  " x)))
                 (car lis))
            (print (car lis)))
        (parse-compiled-code (cdr lis)))))

;; 関数の中で関数適用するとproc, arglistの退避が必要になる.



;; gosh> (parse-compiled-code (compile '(f 'x 'y) 'val 'next))
(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch15))
compiled-branch16
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17



;; これは退避しなくても良い. procから直接procへ入れてるので. (特殊な最適化)
;; gosh> (parse-compiled-code (compile '((f) 'x 'y) 'val 'next))
(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
compiled-branch9
  (assign continue (label proc-return11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
proc-return11
  (assign proc (reg val))
  (goto (label after-call10))
primitive-branch8
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
after-call10
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
compiled-branch13
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14



