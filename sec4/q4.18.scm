;; (p.231) > 内部定義を掃き出すもう一つの戦略は, q4.18.scm に示す. これは「定義された変数の値は, いずれの変数の値も使わずに評価するように強制する」ことである.

;; 以下の書き換えを行う手続きを考える.
;; 注: 訳書の"掃き出す"はscanned outのつもりらしい.

;; before
(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

;; after
(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b)
      <e3>)))

;; a, bは解釈系が作り出した, 利用者のプログラムには現れない新しい変数名.
;; この変換を, 前章のsolve手続きに対して適用することを考える.

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; この内部変数定義を
;;   1. 問題4.18の方法で
;;   2. 本文中の方法で
;; それぞれ変換せよ. 動くか.

;; まず変換手法のある2から. q4.16.scmを利用する. bodyを()で囲って1個のリストにした後食わせる.
;   gosh> (scan-out-defines '((define y (integral (delay dy) y0 dt)) (define dy (stream-map f y)) y))
;   ((let ((y #0='*unassigned*) (dy #0#)) (set! y (integral (delay dy) y0 dt)) (set! dy (stream-map f y)) y))
;; 整理してみる.

(define (solve f y0 dt)
  (let ((y  '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

;; 一方本設問の方法でやったとき(1).
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

;; let内, (b (stream-map f y)) を実行する時点でyが定義されてない(*unassigned*)ためダメ

