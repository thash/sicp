(load "./sec4.2-lazy")

;; driver-loop回してコメントアウトしてる新生cons諸々をvimshellに食わす.

;; 以下はエラーになる.
(car '(a b c))
; => !!!Stack Trace:!!!
;    !!!_______________________________________!!!
;    !!!  0  (eval exp env)!!!
;    !!!        At line 680 of "./sec4.2-lazy.scm"!!!
;    !!!  1  (actual-value input the-global-environment)!!!
;    !!!        At line 441 of "./sec4.2-lazy.scm"!!!
;    !!!!!!(car '(a b c))

;; なんとなれば, クォート式を読み込んで得た"リスト"は, cons, car, cdr定義で得られるリストとは異なるためである.
;;   => クォート式の扱いを新生consたちにあわせてやらねばならない.

;; eval中で ((quoted? exp) (text-of-quotation exp)) となってるからtext-of-quotationをいじる.
;; 元: (define (text-of-quotation exp) (cadr exp))

(define (eval exp env)
        ; ...
        ((variable? exp)(lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env)) ;; changed. pass env.
        ((assignment? exp)(eval-assignment exp env))
        ; ...
        )

(define (text-of-quotation exp env) ;; 引数にenvが加わった.
  (let ((e (cadr exp)))
    (if (pair? e)
      (eval (quote->cons e) env)
      e)))

(define (quote->cons exp)
  (if (pair? exp)
    (list 'cons (quote->cons (car exp)) (quote->cons (cdr exp)))
    (list 'quote exp)))


