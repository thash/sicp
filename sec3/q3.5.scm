;; モンテカルロ積分 -- モンテカルロシミュレーションを使って定積分を見積もる方法
;;
;;   モンテカルロ積分をestimate-integralとして実装せよ。
;;   estimate-integralを使って単位円の面積を測定することで, piの見積もりを出せ。

;; sec1.2.6 から持ってきた, 範囲内でランダムな数を返す手続き. こんな感じの作って使えや、とのこと。
;; (define (random-in-range low high)
;;   (let ((range (- high low)))
;;     (+ low (random range))))

(load "./my_defs")
;; 既存の乱数発生器を使う。乱数発生器としてSRFI-27がいいよと。
;; http://sicp.g.hatena.ne.jp/hyuki/20060505/random
(use srfi-27)

;; random-integerを直接使わないのがキモ
(define (random x)
  (random-integer x))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; (monte-carlo trials some-test) の結果は, trials回数だけiterで回して試行したあとの, experimentが通る確率(割合)になる。
;; condのふたつめみるとわかるが、experimentはtrue/falseを返すものを選ぶ。
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
;exact->inexact

;; "手続きは引数として述語P, 四角形の上限と下限のx1,x2,y1,y2,見積もりを出すために実行する試行の回数をとる"
;; P(x, y)は、打たれた1点が円の中にある(true)かない(false)かを返す手続き。
;; ...であるが、ここではPを引数として受け取るだけなので、estimate-integralを使うときに定義して渡してやる。
;; estimate-integral内の現象としては(test)がtrue/falseを返すことになる
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ((ratio (monte-carlo trials test))
        (square-area (* (- x2 x1) (- y2 y1))))
    (* ratio square-area)))

;; 前述のように(monte-carlo trials test)の結果は範囲0-1に収まる値なので、
;; 四角形の面積にこの割合をかけてやれば円の面積が求まる。定積分を試行から求められたことになる。

;; estimate-integralを使ってpiをestimate. area = r * r * pi
;; 円の中にあるかどうか判別する手続きをP (proc)として定義
;; この推定では、例として単位円(中心(0,0), 半径1の円)を使う。
(define (estimate-pi trials)
  (let ((r 1) (x1 -1) (y1 -1) (x2 1) (y2 1))
    (define (proc x y)
      (<= (* r r) (+ (square x) (square y))))
    (let ((area (estimate-integral proc x1 x2 y1 y2 trials)))
      (exact->inexact (/ area (square r))))))

;; gosh> (estimate-pi 10000)
;; 2.9936, 3.12, 3.2, 3.2, 2.76, 3.08, 3.24, 3.0408...

