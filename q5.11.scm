;; 本文のrestore定義では,
(save y)
(save x)
(restore y)
;; とした時の挙動について特に触れなかった.
;; restoreの実装として以下の3つが考えられる. ちなみに本文の実装は(a)になっている

;; (a). (resotre y) はどのレジスタ出身かは関係なく, stackに退避した最後の値をyに置く.
;; (b). (resotre y) はyとしてsaveした値以外をresotreしようするとエラーになる.
;; (c). (resotre y) はどんな状況であろうと最後に(save y)したyの値を復活する.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 問題(a). (a)の方針が, 5.1.4節(図5.12)のFibonacci計算から1命令を除去するのに使えることを示せ.

;; saveした時と違うregisterにrestoreしても良いという特性を利用したショートカット
(assign continue (label fib-done))
  fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n)
  (assign n (op -) (reg n) (const 1))
  (goto (label fib-loop))
  afterfib-n-1
  (restore n)
  (restore continue)
  (assign n (op -) (reg n) (const 2))
  (save continue)
  (assign continue (label afterfib-n-2))
  (save val)
  (goto (label fib-loop))
  afterfib-n-2
  (assign n (reg val)) ;; *
  (restore val) ;; *
  (restore continue)
  (assign val (op +) (reg val) (reg n))
  (goto (reg continue))
  immediate-answer
  (assign val (reg n))
  (goto (reg continue))
  fib-done)

;; * の2行をまとめて
(restore n)
;; と書くことが出来る. valとしてsaveされている値を直接nに入れてる.
;; valのregisterはその2行下で新しいものに更新されるので問題ない.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 問題(b). (b)を実装せよ. スタックに値と一緒にレジスタ名を置くよう, saveを修正する必要がある.
(define (make-save inst machine stack pc)
  (let ((stack-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine stack-name)))
      (lambda ()
        (push stack (cons stack-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((stack-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine stack-name)))
      (lambda ()
        (let ((top (pop stack)))
          (if (eq? (car top) stack-name)
            (begin
              (set-contents! reg (cdr top))
              (advance-pc pc))
            (error "Cannot restore different name register -- MAKE-RESTORE" stack-name)))))))

;;; 動作確認 ;;;
(define fib-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '+ +) (list '- -))
    '((assign continue (label fib-done))
      fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
      afterfib-n-1
      (restore n)
      (restore continue)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
      afterfib-n-2
      (restore n) ;; (a)で言及した省略版. 怒られるはず
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
      immediate-answer
      (assign val (reg n))
      (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
;; gosh> !!!*** ERROR: Cannot restore different name register -- MAKE-RESTORE n!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 問題(c). (c)を実装せよ. レジスタ毎に個別のstackを用意する必要がある.

;; 複数の解き方を見つけた.
;;     方針(1): stackのs内にregister名付きで個別にstackを生成, assocで見つける.
;;              http://community.schemewiki.org/?sicp-ex-5.11
;;     方針(2): register自身にstackをもたせる
;;              http://d.hatena.ne.jp/rsakamot/20090723/1248328615
;; 変更を最小限にするにはmake-stack, make-save, make-restoreを変えれば事足りる(1)が良いと思ったのでこっちで.

;; s = ((n . (1 2 3 4...) (continue . ((...) (...))))) みたいな構造.

;; make-new-machineでregisterを登録する際に専用stackも一緒に作る.
(define (make-new-machine)
  ;;...
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (begin
           (set! register-table
            (cons (list name (make-register name)) register-table))
           (set! stack (cons (cons name '()) stack)))) ; ++
        'register-allocated)
      ;; ...
      )


(define (make-stack)
  (let ((s '()))
    (define (push reg-name value)
      ;; reg-stack = (n . (1 2 3 ...))
      (let ((reg-stack (assoc reg-name s)))
        (if reg-stack
          (set-cdr! reg-stack (cons value reg-stack))
          (error "Named Stack Not Found -- PUSH" reg-name))))
    (define (pop reg-name)
      (let ((reg-stack (assoc reg-name s)))
        (if reg-stack
          (let ((top (cadr reg-stack))
            (set-cdr! reg-stack (cddr reg-stack))
            top)
          (error "Named Stack Not Found -- POP" reg-name))))
    (define (initialize)
      (for-each
        (lambda (stack) ;; (n . (1 2 3...))のcdr部分をカラに.
          (set-cdr! stack '()))
        s)
      'done)
    (define (show) s) ; my
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'show) (show)) ; my
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

;; (define (pop stack) (stack 'pop)) から
(define (pop stack reg-name) ((stack 'pop) reg-name))
;; (define (push stack value) ((stack 'push) value)) から
(define (push stack reg-name value) ((stack 'push) reg-name value)
