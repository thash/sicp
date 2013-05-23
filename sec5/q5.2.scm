;; 動かす前に書いてみる
;; 本書を通じてcontroller内にデータも書く方式を採用, と言ってるのでそっち.
(controller
  (assign product (const 1))
  (assign counter (const 1))
  test-label
  (test (op >) (reg counter) (reg n))
  (branch (label factorial-done))
  (assign product (op *) (reg product) (reg counter))
  (assign counter (op +) (reg counter) (const 1))
  (goto (label test-label))
  factorial-done)

;; nはどうするか迷った. counst...ではないか. 一回startしたら固定だけど

;; 動作テスト
(load "./sec5.2-A-Register-Machine-Simulator")
(define factorial-machine
  (make-machine
    '(n product counter)
    (list (list '* *) (list '+ +) (list '> >))
    '((assign product (const 1))
      (assign counter (const 1))
      test-label
      (test (op >) (reg counter) (reg n))
      (branch (label factorial-done))
      (assign product (op *) (reg product) (reg counter))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label test-label))
      factorial-done)))
;; 最初 (assign p (const 1)) を (assign p 1) としてしまい以下のエラーが出てた.
;; !!!*** ERROR: Unknown expression type -- ASSENBLE 1!!!

(set-register-contents! factorial-machine 'n 5)
(start factorial-machine)
(get-register-contents factorial-machine 'product)
;; => 120. ok
