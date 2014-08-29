;; 次の手続きのそれぞれを実装するレジスタ計算機を規定せよ.
;; 各計算機に対し, controllerの命令列を書き, データパスを示す図を書け.

;; (a). 再帰的べき乗
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; 再帰を使うのでcounterは必要ない.
;; その代わりにnを入れ替えていく
(controller
  (assign continue (label expt-done)) ; 最終的に戻る場所を最下層にset
  expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case)) ; 再帰終了条件を満たしたらbase-caseへ
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-expt))
  (goto (label expt-loop))
  after-expt
  (restore n)
  (restore continue)
  (assign product (op *) (reg b) (reg product)) ; reg product
  (goto (reg continue)) ; label continueではなくcontinue regに格納されているラベルへ.
  base-case ; 再帰終了条件を満たした時ここへ
  (assign product (const 1)) ; 初期値ここ?
  (goto (reg continue))
  expt-done)


;; 再帰版 -- 動作テスト
(load "./sec5.2-A-Register-Machine-Simulator")
(define expt-machine
  (make-machine
    '(product n b continue)
    (list (list '= =) (list '- -) (list '* *))
    '((assign continue (label expt-done))
       expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-loop))
       after-expt
       (restore n)
       (restore continue)
       (assign product (op *) (reg b) (reg product))
       (goto (reg continue))
       base-case
       (assign product (const 1))
       (goto (reg continue))
       expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 5)
(start expt-machine)
(get-register-contents expt-machine 'product)
; => 32. ok


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (b). 反復的べき乗
;;      こっちは今までと同じ
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

;; controller.
;; n, b をsetしてからstartさせる.
(controller
  (assign p (const 1))
  (assign c (reg n))
  loop-label
  (test (op =) (reg c) (const 0))
  (branch (label expt-done))
  (assign c (op -) (reg c) (const 1))
  (assign p (op *) (reg b) (reg p))
  expt-done)

;; 動作テスト
(load "./sec5.2-A-Register-Machine-Simulator")
(define expt-machine
  (make-machine
    '(n b c p)
    (list (list '= =) (list '- -) (list '* *))
    '((assign p (const 1))
      (assign c (reg n))
      loop-label
      (test (op =) (reg c) (const 0))
      (branch (label expt-done))
      (assign c (op -) (reg c) (const 1))
      (assign p (op *) (reg b) (reg p))
      (goto (label loop-label))
      expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 5)
(start expt-machine)
(get-register-contents expt-machine 'p)
;; => 32. ok
