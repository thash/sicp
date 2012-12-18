;; 次の定義を遅延評価器に入力したとする.

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))
;; 一度w定義するとこでエラった＼(^o^)／
;;   => primitive-procedureに"+"が入ってないからだった. 追加したらいけた.

; ;;; L-Eval input:
; count
;
; ;;; L-Eval value
; 1
;
; ;;; L-Eval input:
; w
;
; ;;; L-Eval value
; 10
;
; ;;; L-Eval input:
; count
;
; ;;; L-Eval value
; 2

;; 途中でthe-global-environmentを確認, 状態を知る.
;; ここの分解(置き換え)がすごくわかりやすい.
;; http://d.hatena.ne.jp/tmurata/20100430/1272628240

