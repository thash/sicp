; 2.3. 記号データ
; 2.3.1. クォート
;
; 任意の記号を用いた合成データ(例: list)

; memq ... trueの時の挙動が特殊なinclude?的なもの
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune)) ; => #f
(memq 'apple '(x (apple sauce) y apple pear)) ; => (apple pear)

; クォートの後はlistにまとめられる。()で囲う必要は無い
; gosh> (list '+ 2 4)
; (+ 2 4)
; gosh> (list '(+ 2 4))
; ((+ 2 4))
