;; (1). 分岐を左から右ではなくランダムな順に探す ramb を実装せよ.

;; Gaucheのリストランダム取得ヒント
;; * http://d.hatena.ne.jp/rui314/20070118/p1
;; * http://practical-scheme.net/gauche/man/gauche-refj_44.html
(use srfi-27)
(define (random x) (random-integer x))
;; (random 3) で 0,1,2 のいずれかが返る

;; itemsからランダムに一つ取ってくる手続き
(define (pick1 items)
  (if (null? items) #f
    (list-ref items (random (length items)))))

;; ambをrambで置き換える. analyze-ambを置き換える形かいな?

;; (2). これが問題4.49 (q4.49.scm) のAlyssaの問題を救うことを示せ.

