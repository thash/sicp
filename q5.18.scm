;; 5.2.1節のmake-registerを修正し, レジスタがトレースできるようにせよ.
;; レジスタはトレース開始と停止するメッセージを受け入れる.
;; レジスタをトレースするときは, レジスタへの値の代入では, レジスタ名, レジスタの古い内容と代入する新しい内容を印字する.
;; 計算機モデルのインターフェイスを拡張し, 指示した計算機レジスタのトレースの開始と停止が出来るようにせよ.

;; わりとかんたんぽい！
;; make-registerを修正. message setのprocに手を入れる.

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-flag #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               ;; setするときに値の変化を出力する
               (if trace-flag (print name " = [" contents "] <- " value)) ; ++
               (set! contents value)))
            ;; trace-on/offを受け入れ, レジスタ個別にtrace on/off出来るようにする.
            ((eq? message 'trace-on) (set! trace-flag #t)) ; ++
            ((eq? message 'trace-off) (set! trace-flag #f)) ; ++
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (register-trace-on  machine reg-name)
  ((get-register machine reg-name) 'trace-on))
(define (register-trace-off machine reg-name)
  ((get-register machine reg-name) 'trace-off))


(define fact-machine
  (make-machine
    '(n val continue)
    (list (list '- -) (list '= =) (list '* *))
    '((assign continue (label fact-done))
      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
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

(register-trace-on fact-machine 'n)
(register-trace-on fact-machine 'val)
(set-register-contents! fact-machine 'n 4)
; n = [*unassigned*] <- 4

; (start fact-machine)

; n = [4] <- 3
; n = [3] <- 2
; n = [2] <- 1
; val = [*unassigned*] <- 1
; n = [1] <- 2
; val = [1] <- 2
; n = [2] <- 3
; val = [2] <- 6
; n = [3] <- 4
; val = [6] <- 24


;; continueとpcが激しいことになるので, trace対象に選択するのはn, valあたりが穏当
;; 激しいことになった出力の例:
; continue = [(((restore n) . #<closure (make-restore make-restore)>) ((restore continue) . #<closure (make-restore make-restore)>) ((assign val (op *) (reg n) (reg val)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>) ((assign val (const 1)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>))] <- ()
; pc = [(((restore continue) . #<closure (make-restore make-restore)>) ((assign val (op *) (reg n) (reg val)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>) ((assign val (const 1)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>))] <- (((assign val (op *) (reg n) (reg val)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>) ((assign val (const 1)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>))
; val = [6] <- 24
; pc = [(((assign val (op *) (reg n) (reg val)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>) ((assign val (const 1)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>))] <- (((goto (reg continue)) . #<closure (make-goto make-goto)>) ((assign val (const 1)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>))
; pc = [(((goto (reg continue)) . #<closure (make-goto make-goto)>) ((assign val (const 1)) . #<closure (make-assign make-assign)>) ((goto (reg continue)) . #<closure (make-goto make-goto)>))] <- ()
