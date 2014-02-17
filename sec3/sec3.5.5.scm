(load "./stream")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.5. 関数的プログラムの部品化度(Modularity)とオブジェクトの部品化度

;;     概要: 3.1.2. - 問題3.6までの議論をストリーム形式で再実装する.
;;
;;     3.1.2. [代入を取り入れた利点]((p.132, PDF p.145) では,
;;         モンテカルロ法を使ってPIを見積もった.
;;         PIの推定方法:
;;           "ランダムに選んだ二つの整数が共通の因子を持たない (coprime = 互いに素 = 最大公約数が1) 確率は6/PI^2である" という事実から.

;; 3.1.2.[代入式を取り入れた利点] で見たように代入の利点の一つは 状態の一部を局所変数にカプセル化(隠匿/hiding)することでシステムのModularityを増大させることが出来たこと.
;;  => ストリームモデルは, 代入を使うことなくModularityを増大させる手法であった.

;; 3.1.2. では代入を使ってこのように書いた.

;    (define rand
;      (let ((x random-init))
;        (lambda ()
;          (set! x (rand-update x))
;          x)))

;; randは手続きを返し, 一度randを定義した後は(rand)を繰り返し呼んで使う.

;; rand-update についてはp.132脚注(sec3.1.2.scm)参照.
;; 線形合同法 による疑似乱数列を生成する.
;;   実装例: http://wqzhang.wordpress.com/2009/09/10/sicp-exercise-3-81/

;; SRFI-27のメルセンヌツイスタを使った疑似乱数の方が良い乱数らしいが,
;; 本文のコードをなるべくそのまま使いたいので
;      良い乱数・悪い乱数
;      http://www001.upp.so-net.ne.jp/isaku/rand.html
;; を参考にCのrand()を真似る

(define random-init 1)
(define (rand-update x)
     (modulo (+ (* x 1103515245) 12345) 2147483647))

;; ストリーム形式においては乱数発生器は存在せず, rand-updateを次々呼び出して作られた乱数ストリームがあるだけ.
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

; gosh> (stream-head random-numbers 10)
; 1, 1103527590, 944465040, 1695244727, 1008001095, 235077491, 776026401, 964188548, 19180611, done

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s))) ;; アタマから2ツ取ってf(ここでは任意)で判断.
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

;; チェザロ実験
;; 二つの自然数をランダムに取るとそれらが互いに素である確率は PI/6 となる.
;; 逆に考えてPIを推測してみよう.
(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1)) ;; 互いに素かどうか
                        random-numbers))

; gosh> (stream-head cesaro-stream 10)
; #t, #t, #t, #t, #t, #t, #f, #t, #t, done

;; monte-carlo 手続きはcesaro-streamを受け取り, 確率の見積もりのストリームを作り出す.
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

; gosh> (define s (monte-carlo cesaro-stream 0 0))
; gosh> (stream-head s 20)
; 1, 1, 1, 1, 1, 1, 6/7, 7/8, 8/9, 9/10, 9/11, 5/6, 11/13, 6/7, 13/15, 7/8, 14/17, 5/6, 16/19, done

;; だんだん理想の値に収束していく

;; 確率ストリームからPIの見積もりストリームへ変換する.
(define pi ;; stream
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

;; ストリーム形式ではもはや試行回数のパラメータは必要ない. 好きなだけPI見積もりストリームを辿れば, その分精度が上がる.
; gosh> (stream-head pi 5)
; 2.449489742783178, 2.449489742783178, 2.449489742783178, 2.449489742783178, done
; gosh> (stream-ref pi 100)
; 2.9011491975882016
; gosh> (stream-ref pi 1000)
; 3.185164088437246
; gosh> (stream-ref pi 10000)
; 3.169840714302888

;; => q3.81.scm, q3.82.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 時の関数型プログラミング的視点

;; 代入を使う 口座モデル
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

;; 初期値 balance と引き出し/預け入れ額ストリーム amount-stream をとり,
;; 銀行口座の残高ストリームを作る.
(define (stream-withdraw balance amount-stream)
  (cons-stream
    balance
    (stream-withdraw (- balance (stream-car amount-stream))
                     (stream-cdr amount-stream))))

;; ストリーム版の銀行口座モデルには代入も局所状態変数も存在しない.

