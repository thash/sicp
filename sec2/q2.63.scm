;; 二進木をリストに変換する
(load "./sec2/sec2.3.3.scm") ;; tree部分をloadする.

;; ざっくり: 再帰なのでオーバーヘッドある. appendが重い
(define (tree->list-1 tree)
          (if (null? tree)
            '()
            (append (tree->list-1 (left-branch tree))
                    (cons (entry tree)
                          (tree->list-1 (right-branch tree))))))
;(trace tree->list-1)

;; 結果を持ちながら最後に内部手続をスタートする反復プロセスなのでかるめ
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
 ; (trace copy-to-list)
  (copy-to-list tree '()))

;; (a). 2つの手続きはすべての木に対して同じ結果を生じるか。そうでなければ、結果はどう違うか。図2.16のような木からどのようなリストを生じるか。

(use gauche.test)
(test-start "q2.63(a)")
(define mytree
  (make-tree 4
             (make-tree 2
                        (make-tree 1 '() '())
                        (make-tree 3 '() '()))
             (make-tree 6
                        (make-tree 5 '() '())
                        (make-tree 7 '() '()))))

(define mytree2
  (make-tree 4
             (make-tree 2
                        (make-tree 1 '() '())
                        (make-tree 3 '() '()))
             (make-tree 6 '() '())))


(test-section "tree->list-1")
(test* "(tree->list-1 mytree)"
       '(1 2 3 4 5 6 7) (tree->list-1 mytree))
(test* "(tree->list-1 mytree2)"
       '(1 2 3 4 6) (tree->list-1 mytree2))

(test-section "tree->list-2")
(test* "(tree->list-2  mytree)"
       '(1 2 3 4 5 6 7) (tree->list-2 mytree))
(test* "(tree->list-2 mytree2)"
       '(1 2 3 4 6) (tree->list-2 mytree2))

(test-end)

;; 結果は同じ.


;; (b). 要素数nのtreeをlistに変換するために必要なコスト増加の違いは？

;; どちらの手続きもすべてのnodeを処理するのでまずn. その1 nodeに対する処理が異なる.
;;   * tree->list-1
;;     append定義はsec2.2.1(p.58)にあり, オーダーは第一引数に比例する. (そうなの?)
;;     tree->list-1の実装で第一引数は (tree->list-1 (left-branch tree)) .
;;     node全体nに対して半分の数. 再帰の度に半分, 半分...になっていくので log n.
;;     よって n * log n でO(n log n).
;;
;;   * tree->list-2
;;     単にconsを呼ぶだけなので定数オーダー.
;;     よってn * 1 で O(n).

;; tree->list-1: O(nlogn)
;; tree->list-2: O(n)

