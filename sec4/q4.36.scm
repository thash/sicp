
;; 無限
;; for i in (amb 1 ... INF)
;;   for j in (amb i ... INF)
;;     for k in (amb j ... INF)
;;     終わりがない

;; 解決策
;; i, jを選んだ時点でkの範囲に制限をかける.

;; kについてひたすら掘っていってしまう. 深さ優先探索なので.
;; grepとか 非決定のまま探していく? ambと同じ部類なの?
;; 非決定性から决定性への変換器...?

