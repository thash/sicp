(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))


; gosh> (define set (list 1 2 3))
; gosh> (define setx (list 1 3 5 7 9))
; gosh> (union-set set setx)
; (2 1 3 5 7 9)
;
; q2.59では、順序を無視。

