;; (p a) が停止する(= エラーで終わったり無限ループにならないこと)かどうかを判定するhalts?を考える.
;;   (define (halts? p a)
;;     ...)
;; halts?が実装可能であると仮定すればこのようなプログラムが実装できる.

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted))

;; 背理法を使う.
;; halts?が実装可能であると仮定すればtryが書ける. tryは内部で(halts? p p)を呼び出している.
;;
;; (try try) の実行結果が
;; 1. "停止する(halted)" であるとき
;;     (halts? try try) はtrueを返すのでtryは(run-forever)を実行し, 停止しない. 矛盾する.
;; 2. "停止しない(run-forever)"であるとき
;;     (halts? try try) はfalseを返すので'haltedを返し, tryの結果が(run-forever)であるという仮定と矛盾する.
;;
;; halts? が実装可能であるという仮定が以上の矛盾を導いた.
;; したがって, halts? は実装不可能である.

