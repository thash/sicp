;; シミュレータに命令トレース(instruction tracing)が出来るように拡張せよ.
;; つまり各命令を実行する前に, シミュレータは命令の文字列を印字する. トレースを開始と停止する
;; trace-onとtrace-offメッセージを計算機モデルが受け入れるようにせよ.

;; q5.15.scm で半分済んでた. あとはon/offできるようにしてif文入れりゃいい
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-count 0)
        (trace-flag #f))
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
            (begin
              (if trace-flag
                (display (list 'executing:: (instruction-text (car insts)) "\n")))
              (set! inst-count (+ 1 inst-count))
              ((instruction-execution-proc (car insts)))
              (execute)))))
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
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (perform (op trace-on)) ; *ココ!*
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (perform (op trace-off)) ; *ココ!*
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done)))

; (set-register-contents! fact-machine 'n 4)
; (start fact-machine)

; (executing:: (restore n)
; )(executing:: (restore continue)
; )(executing:: (assign val (op *) (reg n) (reg val))
; )(executing:: (perform (op trace-off))
; )(executing:: (restore n)
; )(executing:: (restore continue)
; )(executing:: (assign val (op *) (reg n) (reg val))
; )(executing:: (perform (op trace-off))
; )(executing:: (restore n)
; )(executing:: (restore continue)
; )(executing:: (assign val (op *) (reg n) (reg val))
; )(executing:: (perform (op trace-off))
; )done

;; 改行がアレだけど. なおgotoは挟まない方が混乱しなくてよい.

