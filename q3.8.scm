;; 代入を取り入れると、評価モデル(evaluation model)評価する順序を定義しないといけなくなる。
;; 例として単純な手続きfを定義し、(+ (f 0) (f 1))という使い方をしたとき
;; 左から右へ評価すると0を返し、
;; 右から左へ評価すると1を返すようにせよ。

;; ボツ。
(define (f n)
  (let ((state 0))
    (define (switch x)
      (if (eq? x 0) 1 0))
    (begin (set! state (switch state))
           state)
    switch))

;; ボツ。
(define (f n)
  (let ((state 0))
    (define (switch x)
      (let ((prev state))
        (set! state (+ x state))
        state))
    (switch n)))

;; hint: http://wqzhang.wordpress.com/2009/07/13/sicp-exercise-3-8/
;; toggleするswitch/flagにすればいいのか。
(define f
  (let ((state 0))
    (define (switch-state x)
      (let ((old-state state))
        (set! state (+ x state))
        old-state))
    switch-state))

;; (f 0) -> (f 1)
; gosh> (f 0) => 0
; gosh> (f 1) => 0

;; (f 1) -> (f 0)
; gosh> (f 1) => 0
; gosh> (f 0) => 1

;; 遅延足し算か。

