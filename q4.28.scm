;; なんでevalからactual-valueにしたのー？という問題.
;; ネットの噂によれば, 手続きを引数に取る手続きを考えればよさげ.

;; 環境の歩き方
; the-global-environment
; #0=(((test-proc f false true car cdr cons null? + * /) (procedure (x) ((* x 10)) #0#) (procedure (g) ((g 10)) #0#) #f #t (primitive #<subr car>) (primitive #<subr cdr>) (primitive #<subr cons>) (primitive #<subr null?>) (primitive #<subr +>) (primitive #<subr *>) (primitive #<subr />)))

;; lookup-variable-valueで探す.
; gosh> (lookup-variable-value 'f the-global-environment)
; #0=(procedure (g) ((g 10)) #1=(((test-proc f false tr ...
;
; gosh> (lookup-variable-value 'test-proc the-global-environment)
; #0=(procedure (x) ((* x 10)) #1=...


;; ↑のようなfとtest-procを定義.


;;; L-Eval input:
; (f test-proc)

;;; L-Eval value
; 100

;; もしevalだったら

; ;;; L-Eval input:
; (f test-proc)
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (eval exp env)!!!
; !!!        At line 672 of "./sec4.2-lazy.scm"!!!
; !!!  1  (actual-value input the-global-environment)!!!
; !!!        At line 433 of "./sec4.2-lazy.scm"!!!
; !!!!!!(f test-proc)

