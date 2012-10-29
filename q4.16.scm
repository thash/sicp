;; 内部定義を解釈するためにunassigned方式を採る. letが使えると過程.

;; (a). lookup-variable-valueを変更し, 見つけた値が*unassigned*ならエラーにする
(load "./sec4") ;; letを書き加えた.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            ((eq? var '*unassigned*) ;; scan中のcond節に追加
             (error "Unassigned variable" var))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))


;; (b). 手続き本体を取り, 変換を行うことで"内部定義のない等価なもの"を返す手続きscan-out-definesを書け.
;; 内部定義(二重define)をletに展開する処理をひとつの手続きにしてしまう.
;; マクロじゃだめですか.
;; http://sicp-study.g.hatena.ne.jp/papamitra/20070715/sicp_4_16
;;
;; definitionとそれ以外を分離(filter)して, そのあとでくっつけるという解答がネットに多い
(define (scan-out-defines body)
  (define (iter exp)
    (if (null? exp)
      (list '())
      (let ((ret (iter (cdr exp)))
        (clause (car exp)))
      (if (definition? clause)
        (cons (cons
                (list (definition-variable clause) ''*unassigned*)
                (car ret))
              (cons
                (list 'set! (definition-variable clause) (definition-value clause))
                (cdr ret)))
        (cons (car ret) (cons clause (cdr ret)))))))
  (define (include-define? exps)
    (if (null? exps)
      #f
      (if (definition? (car exps))
        #t
        (include-define? (cdr exps)))))
  (if (include-define? body)
    (list (cons 'let (iter body)))
    body))


;; (c). scan-out-defines を make-procedureかprocedure-body(4.1.3)に組み込む. どちらが優れているか.

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
;; =>
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; もしくは

(define (procedure-body p) (caddr p))
;; =>
(define (procedure-body p) (scan-out-defines (caddr p)))

;; procedure-bodyに入れると2回呼ばれるがmake-procedureだと1回なので効率的.

