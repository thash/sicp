(load "./sec2.3.4")
; Huffman木を生成する手続き。
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; "make-code-tree を使い、集合の最小重みの要素を順に合体させ、要素がひとつになったら止める。"
; make-leaf-setによって、((leaf A 1)...)のlistが順序づけられた状態で渡ってくることに注意。
;
; ref: http://d.hatena.ne.jp/bowmoq/20090421/1240268588
; adjoin-set を使う.
(define (successive-merge set)
  (if (null? (cdr set)) (car set)
        (successive-merge  (adjoin-set (make-code-tree (car set) (cadr set))
                                   (cddr set)))))


