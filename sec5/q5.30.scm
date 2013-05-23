;; 現在, 評価器はunknown-expression-typeとunknown-procedure-typeエラーだけを捕捉する.
;; それ以外のエラーが起こると基盤Schemeに戻ってきてしまう(計算機のクラッシュに相当).

;; (a). 未束縛変数へのアクセスエラーを補足するようにせよ.

;; http://www.serendip.ws/archives/3572
;; [根本的] https://github.com/skanev/playground/blob/master/scheme/sicp/05/30.scm
