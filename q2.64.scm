; (load "./q2.63")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; 二分する処理
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

; (a). partial-treeがどう働くか、出来るだけ明快な短い説明を書け。list->treeがリスト(1 3 5 7 9 11)に対して作る木を書け。
;   => [描いたよ](http://www.evernote.com/shard/s11/sh/de3edca4-4d48-489b-a223-096dfee3088e/4cd00e29c4a14f6dc4cb7fbc41378775)
; (b). list->treeがn個の要素のlistを変換するために必要なステップ数の増加の程度(オーダー?)はどの程度か

(use gauche.test)
(load "./my_defs")
(load "./sec2.3.3-tree")
(test-start "q2.64")
(test-section "misc.")
(test* "(quotient 12 3)" '4 (quotient 12 3))
(test* "(quotient 12 5)" '2 (quotient 12 5))

; (test-section "partial-tree")
(test-section "list->tree")
(eqt '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) (list->tree '(1 3 5 7 9 11)))
(test-end)

