;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.4. ストリームと遅延評価
(load "./sec3.5.3") ; include stream, my_defs


; (define int
;   (cons-stream initial-value
;                (add-streams (scale-stream integrand dt) int)))


;; 微分方程式を解く solve 手続きを作ってみるが,
;; yとdyの定義がお互いを参照しているため, 動かない.
;
;    (define (solve f y0 dt)
;      (define y (integral dy y0 dt))
;      (define dy (stream-map f y))
;      y)
;
;; ところが, 原理的には引数の一部が分かっていれば評価し始めることはできる.
;; 遅延評価の出番. 次のようにintegralを再定義する.

;; delayed-integrandに注目.
(define (integral delayed-integrand initial-value dt)
  (define int (cons-stream initial-value
                           (let ((integrand (force delayed-integrand)))
                             (add-streams (scale-stream integrand dt)
                                          int))))
  int)

;; このように定義したintegralを使えば,
;; integralに渡すdyにdelayをかけdelayed-objectとして扱うことで solveを動かすことが出来る.

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; テスト
; (display-stream-n (solve (lambda (y) y) 1 0.001) 20)
; (newline)
; (display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))

;; fの形が f(y) = y <=> dy/dt = y. 微分しても形が変わらないといえば底がeの指数関数e^x.
;; 1000分の1刻みで1000個目, つまりt=1を取るとeそのもの(2.71...)が求められる.


;; => q3.77.scm, q3.78.scm, q3.79.scm, q3.80.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 正規順序の評価 (Normal-order evaluation)

;; delayとforceで柔軟なプログラムが書けるようになったが, そのぶん複雑さも増した.
;; integralの再実装で見たように, 「通常の手続き」と「遅延引数を取る手続き」の2クラスに分けられるようになった.
;; 手続きの種類を統一するための一つの方法は, すべての手続きが遅延引数を取るようにすることである.
;; 手続きのすべての引数が自動的に遅延オブジェクトになり, 本当に必要になったときforceで評価されるような設計.
;; この設計は, 1.1.5. で述べた正規順序の評価に相当し, 実際に 4.2. で言語をこのように変更する.
;;
;; ところがこの設計だと代入システムとうまく共存できない.
;; 問題3.51, 3.52で見たように遅延評価と代入が混在するとかなり複雑になり混乱をもたらす. というわけで2つを同時に扱うのは熱い研究領域らしい.


