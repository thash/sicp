;; 監視つきstackを使い, 評価器の末尾再帰的特性(5.4.2節)を検討せよ.
;; 1.2.1節の反復的factorial手続きを定義せよ.
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;; いくつかの小さいnの値につき手続きを走らせ, 各値のn!計算に必要なスタックの最大深さとプッシュの回数を記録せよ.
;; (a). n!の評価に必要な最大深さはnに無関係であることを発見せよ.
;; (b). データからn>=1なるn!の評価に使ったプッシュ演算の総数のnに関する式を求めよ.
;;      使った演算の数はnの線形関数で, 2つの定数で決まることに注意せよ.

(load "./sec5.4")
(start eceval)

;; total-pushesの統計情報を表示させるのどうすんだっけ...
;; q5.14.scm で使ってるけどcontrollerに埋め込むスタイルで評価器ver考えてない.

;; [没] 補助手続きをいくつか定義...していたが
;; そのまま使うとstartでinput待ちで止まってしまう
(define (stack-stats machine exp)
  (set-register-contents! machine 'exp exp)
  ((machine 'stack) 'initialize)
  (start machine)
  ((machine 'stack) 'print-statistics))


;; 解決策:
;; print-resultを改修. sec5.4のREPL部分, (op user-print)の後に
;; (perform (op print-stack-statistics))を追加した. これで出力時に統計情報が一緒に出てくる

(factorial 1)
;; => 1
;; => (total-pushes = 64 maximum-depth = 10)
(factorial 2)
;; => 2
;; => (total-pushes = 99 maximum-depth = 10)
(factorial 3)
;; => 6
;; => (total-pushes = 134 maximum-depth = 10)
(factorial 4)
;; => 24
;; => (total-pushes = 169 maximum-depth = 10)
(factorial 5)
;; => 120
;; => (total-pushes = 204 maximum-depth = 10)
(factorial 10)
;; => 3628800
;; => (total-pushes = 379 maximum-depth = 10)
(factorial 30)
;; => 265252859812191058636308480000000
;; => (total-pushes = 1079 maximum-depth = 10)

;; (a). max = 10

;; (b).
;; 64, 99, 134, 169, 204, n6..n9, 379...
;; よって total-pushes = 35 * n + 29
