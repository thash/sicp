;; いままでは重複なしの前提で集合を組み立ててきたが,
;; 仮に重複を許すとするとどうなるか.

;; [element-of-set?] sec2.3.3.scm と変化なし.

;; [adjoin-set] あってもなくてもくっつける
(define (adjoin-set x set) (cons x set))

;; [intersection-set]
; set2に要素があったらset2を返すのではなく、set2に要素を加えて返す
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
           (cons (car set1) (intersection-set (cdr set1) (adjoin-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

    (intersection-set '(a b c b) '(b c)) ;; => (b c b)
    ;; 別にsec2.3.3のintersection-setでも同じ結果だな...

; set1の内容をつぎつぎset2に移していくだけ. element-of-set?を判定する必要が無いため効率はよくなる。
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

    (union-set '(1 2 3) '(1 3 5 7 9)) ;; => (3 2 1 1 3 5 7 9)

; 重複ありバージョンが使いたくなる例としては、共通して使われている数を集計したいときとか？

; => 勉強会にて
; スペースを食ってしまっても良いけどupdate/insertを速くしたい、というケースに良い。
; 例えば、Postgresは追記していって後でvacuumするという設計。

; intersection-setがうまくいかない。答えが(1 3)と、sec2.3.3と同じになる。(1 1 3 3)となってほしい(そもそもこの考えは合ってる？)。


