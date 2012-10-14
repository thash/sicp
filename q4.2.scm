;; いらんことしいのLouisたんがeval中のcond順序を変えればよくねと言う.
;; 手続き作用(apply)の節を代入より前に持ってきたほうが効率が良いなどと供述しており

; gosh> (load "./sec4")
; #t
; gosh> (driver-loop)
; ;;; M-Eval input:
; (define x 3)
; ;;; M-Eval value
; ok
;;; M-Eval input:

;; Louisの主張する順序に入れ替えたeval.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))

        ;; ++
        ((application? exp)
         (apply (eval-louis (operator exp) env)
                (list-of-values (operands exp) env)))

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
        (else
          (error "Unknown expression type -- EVAL" exp))))


; gosh> (load "./sec4-louis")
; ;;; M-Eval input:
; (define x 3)
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (eval (operator exp) env)!!!
; !!!        At line 63 of "./sec4-louis.scm"!!!
; !!!  1  (eval input the-global-environment)!!!
; !!!        At line 375 of "./sec4-louis.scm"!!!
; !!!!!!(define x 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 後半(b)

;; application?の定義を変更してやる. 引きづられてoperatorとoperandも, callをまたぐようにする.

;; 元コード: (define (application? exp) (pair? exp))
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

; gosh> (load "./sec4-louis")
; #t
; gosh> (driver-loop)
; ;;; M-Eval input:
; (define  x 3)
; ;;; M-Eval value
; ok
; ;;; M-Eval input:
; x
; ;;; M-Eval value
; 3
;
;
;; ここまではok

;; 使い方が変わったからこれもエラーで良いとして,
; gosh> (driver-loop)
; ;;; M-Eval input:
; (+ 1 2)
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (eval input the-global-environment)!!!
; !!!        At line 379 of "./sec4-louis.scm"!!!
; !!!!!!(+ 1 2)

;; あれ, callも使えないのはなぜ
; gosh> (driver-loop)
; ;;; M-Eval input:
; (call + 1 2)
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (eval (operator exp) env)!!!
; !!!        At line 63 of "./sec4-louis.scm"!!!
; !!!  1  (eval input the-global-environment)!!!
; !!!        At line 379 of "./sec4-louis.scm"!!!
; !!!!!!(call + 1 2)


