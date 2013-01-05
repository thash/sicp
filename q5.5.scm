;; 階乗とFibonacci計算機を"机上シミュレート"せよ
;;   => sec5.1-Designing-Register-Machines.scm L130あたり
;; 手で書いて見るのが一番


;;;; 動作を確認する
;; 階乗
(load "./sec5.2-A-Register-Machine-Simulator")
(define fact-machine
  (make-machine
    '(n val continue)
    (list (list '= =) (list '- -) (list '* *))
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

(set-register-contents! fact-machine 'n 6)
(start fact-machine)
(get-register-contents fact-machine 'val)
; => 720. ok


;; Fibonacci
(load "./sec5.2-A-Register-Machine-Simulator")
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
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
      immediate-answer
      (assign val (reg n))
      (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
(get-register-contents fib-machine 'val)
; -> 8. ok
