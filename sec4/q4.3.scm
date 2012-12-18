;; データ主導(data-directed)の形式になるようevalを書きなおせ.
;; -> sec2/q2.73.scm のデータ主導微分プログラムと比較.

;; 要するにeval中でcondによる条件分岐をするんじゃなくて,
;; 渡されたexpの単語でtableからoperatorを検索して適用(apply).
;;     (get 'deriv (operator exp))

;; sec3/sec3.3.3.scm より -- make-table, lookup, assoc, insert! {{{
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table) (list '*table*)) ;; }}}

;; この文脈化でget/putはeval-tableの操作をするものと固定
(define eval-table (make-table))
(define get (lambda (key) (lookup key eval-table)))
(define put (lambda (key proc) (insert! key proc eval-table)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))

        ;; get(=lookup)は対象がなければfalseを返すのでこのように使える.
        ((get (car exp)) ((get (car exp)) exp env))

        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

;; sec4.scm で定義されたtagged-list?対象は以下のとおり.これらをtableにぶちこむ.
;     (define (quoted? exp)     (tagged-list? exp 'quote))
;     (define (assignment? exp) (tagged-list? exp 'set!))
;     (define (definition? exp) (tagged-list? exp 'define))
;     (define (lambda? exp)     (tagged-list? exp 'lambda))
;     (define (if? exp)         (tagged-list? exp 'if))
;     (define (begin? exp)      (tagged-list? exp 'begin))
;     (define (cond? exp)       (tagged-list? exp 'cond))
;; procに当たるのはそれぞれeval中のcondで実行文として与えられていたもの.

(put 'quote  (lambda (exp env) (text-of-quotation exp))))
(put 'set!   (lambda (exp env) (eval-assignment exp env)))
(put 'define (lambda (exp env) (eval-definition exp env)))
(put 'if     (lambda (exp env) (eval-if exp env)))
(put 'begin  (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'cond   (lambda (exp env) (eval (cond->if exp) env)))
(put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env)))

