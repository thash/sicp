(load "./sec4.4-Serendip")
;(query-driver-loop)

;; reverse演算を実装せよ.
;; 想定される使い方
(reverse (1 2 3) ?x)
(reverse ?x (1 2 3))

;; append-to-form -> sec4.4-Logic-Programming.scm "プログラムとしての論理" より抽出
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

;;; 実装 ;;;
(assert! (rule (reverse (?x) (?x)))) ;; 最終防御用pattern match
(assert! (rule (reverse (?head . ?tail) ?z)
               (and (reverse ?tail ?w)
                    (append-to-form ?w (?head) ?z))))

;; 動いた.
;; が, これだと (reverse (1 2 3) ?x) しか対応できない.

