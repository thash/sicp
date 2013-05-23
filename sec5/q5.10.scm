;; レジスタ計算機命令に新しい構文を設計し, シミュレータがその新しい構文を使えるように設計せよ.
;; 本節の構文手続き以外のシミュレータ部分を変更せず, 新しい構文を実装できるか.
;; Design a new syntax for register-machine instructions and modify the simulator to use your new syntax. Can you implement your new syntax without changing any part of the simulator except the syntax procedures in this section?

;; make-execution-procedure内のcond節に新しい判定を追加し, 対応するmake-*を実装すればいい

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign  inst machine labels ops pc))
        ;; ...
        ((eq? (car inst) 'show-stack)
         (make-show-stack stack pc))
        ;; ...
        (else (error "Unknown instruction type -- ASSENBLE"
                     inst))))

(define (make-show-stack stack pc)
  (lambda ()
    (display "\nstack :: ")
    ;; 現在のstackを出力
    ;; (下準備としてmake-stackにsを返すだけのshow手続きを定義&dispatchした)
    (display (show stack))
    (display "\n\nnext -> ")
    ;; 直後に実行される命令文を表示
    (display (caar (cdr (get-contents pc))))
    (display "\n---------\n")
    (advance-pc pc)))

;;; 動作テスト ;;;
;; save/restoreでstackを使っている例
(define fib-machine
  (make-machine
    '(n val continue)
    (list (list '- -) (list '= =) (list '* *))
    '((assign continue (label fact-done))
      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (show-stack)
      (save continue)
      (show-stack)
      (save n)
      (show-stack)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (show-stack)
      (restore n)
      (show-stack)
      (restore continue)
      (show-stack)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done)))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)

;; 結構使える
