;; q5.16.scm の命令トレースを拡張し, シミュレータが命令を印字する前に制御器の命令の直前にあるラベルを印字するようにせよ.
;; 命令の係数(q5.15.scm)に干渉せずに行うよう注意せよ.
;; シミュレータが必要なラベル情報を保存するようにしなければならない.

;; 現状, シミュレータはラベル情報を持ってない.
;; registerでcurrent-labelを持たせる?
;;   => 命令列にassignを追加する必要がありinst-countを増加させるため不可.
;; machine内変数でcurrent-labelを持たせる?
;; gotoやbranchで移動するところは捕捉しやすいけど, しれっとlabel超えて実行している部分はどうする...


;; 引き続きmake-new-machineを拡張していくが,
;; まずそもそもの問題として, execute時のinstsにlabel情報が含まれていない.
;; なぜなら, extract-labelsでlabelの時instsを何もいじらずそのまま渡していたから.
;; ここを変更し, instsにlabelがあったことを記録させる.
(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          ;; (receive insts... を変更. instsを変更なしに次へ渡すのではなく,
                          ;; label以外では(text . '(proc予定地))としているところに, (label . label-name)をくっつける.
                          (receive (cons (cons 'label next-inst) insts)
                                   (cons (make-label-entry next-inst
                                                           insts)
                                         labels))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))
;; 既存実装はextract-labelsでlabelと命令を分けてる.
;; それを, 命令の方にもlabelを残して, 切り替わった時に(labelを通過した時に)くっつけるようにする(?)

;; instsにlabelも入ってくるので処理をskipする
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (if (eq? (car inst) 'label) ; ++
          'skip-label ; ++
          (set-instruction-execution-proc!
            inst
            (make-execution-procedure
              (instruction-text inst) labels machine
              pc flag stack ops))))
      insts)))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond  ((symbol? (car inst)) inst) ; ++
         ((eq? (car inst) 'assign)
          (make-assign  inst machine labels ops pc))
         ((eq? (car inst) 'test)
          (make-test    inst machine labels ops flag pc))
         ((eq? (car inst) 'branch)
          (make-branch  inst machine labels flag pc))
         ((eq? (car inst) 'goto)
          (make-goto    inst machine labels pc))
         ((eq? (car inst) 'save)
          (make-save    inst machine stack pc))
         ((eq? (car inst) 'restore)
          (make-restore inst machine stack pc))
         ((eq? (car inst) 'perform)
          (make-perform inst machine labels ops pc))
         ((eq? (car inst) 'show-stack)
          (make-show-stack stack pc))
         (else (error "Unknown instruction type -- ASSENBLE"
                      inst))))


(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-count 0)
        (trace-flag #f)
        (label '())) ; ++
    (let
      ((the-ops
         (list (list 'initialize-stack
                     (lambda () (stack 'initialize)))
               (list 'print-stack-statistics
                     (lambda () (stack 'print-statistics)))
               (list 'initialize-inst-count
                     (lambda () (set! inst-count 0)))
               (list 'print-inst-count
                     (lambda () (display (list 'inst-count '= inst-count)) (newline)))
               (list 'trace-on  (lambda () (set! trace-flag #t)))
               (list 'trace-off (lambda () (set! trace-flag #f))))) ;; trace-on/offをoperationとして追加
       (register-table
         (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            ;; 変更>>> xxxだめ
            (begin
              (display (caar insts))
              (newline)
              (if (eq? (caar insts) 'label)
                (begin
                  (if trace-flag
                    (begin (display (car insts)) (newline)))
                  (advance-pc pc)
                  (execute)))
              (begin
                ;(newline)
                ;(display (car insts))
                (if trace-flag
                  (display (list 'executing:: (instruction-text (car insts)) "\n")))
                (set! inst-count (+ 1 inst-count))
                ((instruction-execution-proc (car insts)))
                (execute)))))) ;; <<< 変更

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; 実行テスト
(define fact-machine
  (make-machine
    '(n val continue)
    (list (list '- -) (list '= =) (list '* *))
    '((assign continue (label fact-done))
      fact-loop
      (perform (op trace-on)) ; ++
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (perform (op trace-off)) ; ++
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done)))

(set-register-contents! fact-machine 'n 4)
(start fact-machine)
