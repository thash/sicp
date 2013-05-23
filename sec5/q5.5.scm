;; 階乗とFibonacci計算機を"机上シミュレート"せよ
;;   => sec5.1-Designing-Register-Machines.scm L130あたり
(controller
 1.  (assign continue (label fact-done))
 2.  fact-loop
 3.  (test (op =) (reg n) (const 1))
 4.  (branch (label base-case))
 5.  (save continue)
 6.  (save n)
 7.  (assign n (op -) (reg n) (const 1))
 8.  (assign continue (label after-fact))
 9.  (goto (label fact-loop))
10.  after-fact
11.  (restore n)
12.  (restore continue)
13.  (assign val (op *) (reg n) (reg val))
14.  (goto (reg continue))
15.  base-case
16.  (assign val (const 1))
17.  (goto (reg continue))
18.  fact-done)

;; 状態を書いていく. n=3として
;; 1,3,5,6
n: 3
val: --
continue: fact-done
stack: fact-done 3

;; 7,8
n: 2
val: --
continue: after-fact
stack: fact-done 3

;; 9-goto->2, 3, 5, 6
n: 2
val: --
continue: after-fact
stack: fact-done 3 after-fact 2

;; 7,8
n: 1
val: --
continue: after-fact
stack: fact-done 3 after-fact 2

;; 9-goto->2, 3, "4"-branch->15,16
n: 1
val: 1
continue: after-fact
stack: fact-done 3 after-fact 2

;; 17-goto->10,11,12,13
n: 2
val: 2
continue: after-fact
stack: fact-done 3

;; 14-goto->10,11,12,13,14-goto->18(fact-done)
n: 3
val: 6
continue: fact-done
stack:

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fibonacci
(controller
 1.  (assign continue (label fib-done))
 2.  fib-loop
 3.  (test (op <) (reg n) (const 2))
 4.  (branch (label immediate-answer))
 5.  (save continue)
 6.  (assign continue (label afterfib-n-1))
 7.  (save n)
 8.  (assign n (op -) (reg n) (const 1))
 9.  (goto (label fib-loop))
10.  afterfib-n-1
11.  (restore n)
12.  (restore continue)
13.  (assign n (op -) (reg n) (const 2))
14.  (save continue)
15.  (assign continue (label afterfib-n-2))
16.  (save val)
17.  (goto (label fib-loop))
18.  afterfib-n-2
19.  (assign n (reg val))
20.  (restore val)
21.  (restore continue)
22.  (assign val (op +) (reg val) (reg n))
23.  (goto (reg continue))
24.  immediate-answer
25.  (assign val (reg n))
26.  (goto (reg continue))
27.  fib-done)

;; 状態を書いていく. n=3として
;; 1,3,5
n: 3
val: --
continue: fib-done
stack: fib-done

;; 6,7,8
n: 2
val: --
continue: afterfib-n-1
stack: fib-done 3

;; 9-goto->2,3,5
n: 2
val: --
continue: afterfib-n-1
stack: fib-done 3 afterfib-n-1

;; 6,7,8
n: 1
val: --
continue: afterfib-n-1
stack: fib-done 3 afterfib-n-1 2

;; 9-goto->2,3,"4"-branch->24(immediate-answer),25
n: 1
val: 1
continue: afterfib-n-1
stack: fib-done 3 afterfib-n-1 2

;; 26-goto-> 10(afterfib-n-1),11,12
n: 2
val: 1
continue: afterfib-n-1
stack: fib-done 3

;; 13,14,15,16
n: 0
val: 1
continue: afterfib-n-2
stack: fib-done 3 afterfib-n-1 1

;; 17-goto->2,3,"4"->branch->24(immediate-answer),25
n: 0
val: 0
continue: afterfib-n-2
stack: fib-done 3 afterfib-n-1 1

;; 26-goto->18(afterfib-n-2),19,20,21
n: 0
val: 1
continue: afterfib-n-1
stack: fib-done 3

;; 22
n: 0
val: 1 ; 1 + 0 = 1
continue: afterfib-n-1
stack: fib-done 3

;; 23-goto->10(afterfib-n-1),11,12
n: 3
val: 1
continue: fib-done
stack:

;; 13,14,15,16
n: 1
val: 1
continue: afterfib-n-2
stack: fib-done 1

;; 17-goto->2(fib-loop),3,"4"-branch->24(immediate-answer),25
n: 1
val: 1 ; val = n
continue: afterfib-n-2
stack: fib-done 1

;; 26-goto-> 18,19,20,21
n: 1 ; n = val
val: 1 ; restore 1
continue: fib-done
stack:

;; 22,23-goto-> fib-done
n: 1
val: 2 ; 1 + 1
continue: fib-done
stack:

;; 1,1,2,3,5,8..だからfib(6)くらい行かないと面白味ないんだけどn=3でこのステップ数


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
