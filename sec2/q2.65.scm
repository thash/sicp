;; いっぺんlistにしてからunion-set, treeに戻す
(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (union-set list1 list2))))

(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (intersection-set list1 list2))))
