;; 1.1.7節で述べたNewton法を使い, 平方根を計算する計算機を設計せよ
;; Newton法によるsqrtの計算
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; * 計算機が覚えておく必要のある変数
;;     x, guess
;; * 必要な基本演算
;;     <, abs, -, square, average
;; * サイクル内でのレジスタへの値格納
;;     guess <- (improve guess)

;; データパス図
;; https://www.evernote.com/shard/s11/sh/348a5375-0022-434e-b856-20fcb117016e/cbd6b01b46b17dd6398a1f3337f10231

;; controllerを書こう.
;;   * 一時変数を使う方針で.
;;   * average, squareは基本演算として用意してやる

(controller
  (assign g (const 1.0))
  test-label
  (assign tmp1 (op square) (reg g))
  (assign tmp2 (op -) (reg tmp1) (reg x))
  (assign tmp3 (op abs) (reg tmp2))
  (test (op <) (reg tmp3) (const 0.001))
  (branch (label sqrt-done))
  (assign gtmp1 (op /) (reg x) (reg g))
  (assign g (op average) (reg g) (reg gtmp1))
  (goto (label test-label))
  sqrt-done)


;; 動作テスト
(load "./sec5.2-A-Register-Machine-Simulator")
(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define sqrt-machine
  (make-machine
    '(x g tmp1 tmp2 tmp3 gtmp1 gtmp2)
    (list (list '< <) (list '/ /) (list '- -)
          (list 'abs abs) (list 'square square) (list 'average average))
    '((assign g (const 1.0))
      test-label
      (assign tmp1 (op square) (reg g))
      (assign tmp2 (op -) (reg tmp1) (reg x))
      (assign tmp3 (op abs) (reg tmp2))
      (test (op <) (reg tmp3) (const 0.001))
      (branch (label sqrt-done))
      (assign gtmp1 (op /) (reg x) (reg g))
      (assign g (op average) (reg g) (reg gtmp1))
      (goto (label test-label))
      sqrt-done)))
;; 最初, (op average) の引数1個忘れて エラー出てた. value-proc!!! で怒られるので何かと思ったが.

(set-register-contents! sqrt-machine 'x 3)
(start sqrt-machine)
(get-register-contents sqrt-machine 'g)
;; => 1.7321428571428572 ok!
