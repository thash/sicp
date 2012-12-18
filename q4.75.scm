;; qevalした結果がただひとつのstreamだったら成功. ソレ以外だったら失敗.


(unique (job (?x (computer programmer))))
;; だとダメ
(unique (job (?x (computer wizard))))
;; だと結果出る


