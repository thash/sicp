(load "./stream")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.5. 関数的プログラムの部品化度(Modularity)とオブジェクトの部品化度

;; 3.1.2.[代入式を取り入れた利点] で見たように代入の利点の一つは 状態の一部を局所変数にカプセル化(隠匿/hiding)することでシステムのModularityを増大させることが出来たこと.
;; ストリームモデルも代入を使うことなくModularityを増大させる手法であった.
;; そこで 3.1.2. で(Modularityの実例として使った)モンテカルロ法によるPIの見積もりをストリームで書いてみよう.

;; 3.1.2. ではこのように書いてた.
; (define rand
;   (let ((x random-init))
;     (lambda ()
;       (set! x (rand-update x))
;       x)))

;; ストリーム形式においては乱数発生器は存在せず, rand-updateを次々呼び出して作られた乱数ストリームがあるだけ.

;; http://wqzhang.wordpress.com/2009/09/10/sicp-exercise-3-81/
(define random-init 137)
(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

;; monte-carlo 手続きはcesaro-streamを受け取り, 確率の見積もりのストリームを作り出す.
;; その後確率ストリームからPIの見積もりストリームへ変換する. (この処理はpiの定義中で行われる)
;; ストリーム形式ではもはや試行回数のパラメータは必要ない. 好きなだけPI見積もりストリームを辿ればその分精度が上がる.


;; => q3.81.scm, q3.82.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 時の関数型プログラミング的視点

;; ...

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

;; ...

(define (stream-withdraw balance amount-stream)
  (cons-stream
    balance
    (stream-withdraw (- balance (stream-car amount-stream))
                     (stream-cdr amount-stream))))

;; stream版の銀行口座モデルには代入も局所状態変数も存在しない.

;; 動かしてみる.



