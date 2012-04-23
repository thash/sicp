; 重複を許すバージョン。

; element-of-set?は同じ手続き。
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; adjoin-set. あってもなくてもくっつける
(define (adjoin-set x set) (cons x set))

; set2に要素があったらset2を返すのではなく、set2に要素を加えて返す
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
           (cons (car set1) (intersection-set (cdr set1) (adjoin-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

; set1の内容をつぎつぎset2に移していくだけ. element-of-set?を判定する必要が無いため効率はよくなる。
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

; (define set (list 1 2 3))
; (define setx (list 1 3 5 7 9))
;
; gosh> (union-set set setx)
; (3 2 1 1 3 5 7 9)

; 重複ありバージョンが使いたくなる例としては、共通して使われている数を集計したいときとか？

; => 勉強会にて
; スペースを食ってしまっても良いけどupdate/insertを速くしたい、というケースに良い。
; 例えば、Postgresは追記していって後でvacuumするという設計。

; intersection-setがうまくいかない。答えが(1 3)と、sec2.3.3と同じになる。(1 1 3 3)となってほしい(そもそもこの考えは合ってる？)。


