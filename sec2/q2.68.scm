(load "./q2.67")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; usage:
; gosh> (symbol-included? 'B (symbols sample-tree))
(define (symbol-included? symbol tree-symbols)
  ;(if (or (null? tree-symbols) (null? (cdr tree-symbols))) #f
  (if (null? tree-symbols) #f
    (if (eq? (car tree-symbols) symbol) #t
      (symbol-included? symbol (cdr tree-symbols)))))


; 分岐点が配下のすべてのsymbolを持つことで、すべて走査しなくても符号化可否が最初に分かるようになっている
; leafまで到達した時点で目的は達している。

(define (encode-symbol symbol tree)
  (if (leaf? tree)
    '()
     (if (symbol-included? symbol (symbols (left-branch tree)))
       (cons 0 (encode-symbol symbol (left-branch tree)))
       (cons 1 (encode-symbol symbol (right-branch tree))))
    ))

;初回, そもそも含まれる文字が問われているかどうかを見たいんだけど、二週目以降冗長になる...
;(if (symbol-included? symbol (symbols tree))
;(print "no symbol")

