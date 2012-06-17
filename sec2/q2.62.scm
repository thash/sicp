; 順序付きunion-set

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      (else
                        (cons x2 (union-set set1 (cdr set2)))))))))

; (define s1 '(1 3 6))
; (define s2 '(2 3 4 7))
; gosh> (union-set s1 s2)
; (1 2 3 4 6 7)


