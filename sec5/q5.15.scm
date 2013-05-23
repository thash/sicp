;; レジスタ計算機シミュレーションに, 命令形数(instruction counting)を追加せよ.
;; つまり計算機モデルに実行した命令の回数を覚えさせる.
;; 計算機モデルのインターフェイスを拡張し,
;; 命令回数の値を印字し, 回数をゼロに設定するメッセージを受け入れるようにせよ.

;; make-new-machineに手を入れる. executeでinstsを実行する度にカウントを増やす.
;; NEXT-LOOP: opとして定義するのではなくmachine自体がメッセージを受け入れるようにする別解も.
;;            (たぶんそれだと命令文の途中で値を見ることが出来ないけど).
;;            あとは, registerとして実装する方法もある.
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-count 0))
    (let
      ((the-ops
         (list (list 'initialize-stack
                     (lambda () (stack 'initialize)))
               (list 'print-stack-statistics
                     (lambda () (stack 'print-statistics)))
               (list 'initialize-inst-count
                     (lambda () (set! inst-count 0)))
               (list 'print-inst-count
                     (lambda () (display (list 'inst-count '= inst-count)) (newline)))))
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
              ;; DEBUG
              ;; (display (list 'executing:: (instruction-text (car insts))))
              ;; (newline)
              (set! inst-count (+ 1 inst-count)) ;; *ココ!*
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
    ;; 最初にstack/countを初期化するようにすればmachineの再定義が不要に.
    '((perform (op initialize-stack))
      (perform (op initialize-inst-count))
      (assign continue (label fact-done))
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
      fact-done
      (perform (op print-stack-statistics))
      (perform (op print-inst-count))))) ;; 最後に出力を追加

; (set-register-contents! fact-machine 'n 5)
; (start fact-machine)
;; (total-pushes = 8 maximum-depth = 8)
;; (inst-count = 51)

;; n = 10 -> 20で2桁増加.
;; performによるdebug命令もカウントされてしまうので注意.

