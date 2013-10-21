(load "./sec2/q2.68.scm")
; Huffman木を生成する手続き。
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; "make-code-tree を使い、集合の最小重みの要素を順に合体させ、要素がひとつになったら止める。"
; make-leaf-setによって、((leaf A 1)...)のlistが順序づけられた状態で渡ってくることに注意。
;
; ref: http://d.hatena.ne.jp/bowmoq/20090421/1240268588
; adjoin-set を使う.
(define (successive-merge set)
  (if (null? (cdr set)) ;; 最終的に要素が1個になったら終了
      (car set)
      (let ((first  (car set))   ;; 可読性のために名前付け
            (second (cadr set))
            (rest   (cddr set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      rest)))))

;; successive-mergeを読んだ時点で小さい順に並んでいる

