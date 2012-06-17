; 二進木をリストに変換する
(load "./sec2.3.3-tree")

(define (tree->list-1 tree)
          (if (null? tree)
            '()
            (append (tree->list-1 (left-branch tree))
                    (cons (entry tree)
                          (tree->list-1 (right-branch tree))))))
;(trace tree->list-1)

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

; (a). 2つの手続きはすべての木に対して同じ結果を生じるか。そうでなければ、結果はどう違うか。図2.16のような木からどのようなリストを生じるか。
; (b). 要素数nのtreeをlistに変換するために必要なコスト増加の違いは？

; ----------------

(use gauche.test)
(test-start "q2.63")
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

