;(define (fringe items)
;  (if (null? items)
;    ()
;    (if (list? #?=items)
;      (list (fringe (car items)) (fringe (cadr items)))
;      items
;      )))


; (define (fringe items)
;   (cond ((not (list? (car items)))
;          (if (null? (cdr items)) (car items)
;            (list (car items) (fringe #?=(cdr items)))))
;         (else (if (null? (cdr items)) (fringe (car items))
;                 (cons (fringe (car items)) (fringe (cdr items)))))
;         ))

; (define (fringe items)
;   (if (null? items) ()
;     (cond ((and (not (pair? (car items))) (not (pair? (cdr items))))
;            (car items))
;           ((and (not (pair? (car items))) (pair? (cdr items)))
;            (cons (car items) (fringe (cdr items))))
;           (else
;             (cons (fringe (car items)) (fringe (cdr items))))
;           )))

; わからん
; => http://wiki.drewhess.com/wiki/SICP_exercise_2.28
;    list と appendを使っている。


(define (fringe items)
  (if (null? items) ()
    (cond ((and (not (pair? (car items))) (not (pair? (cdr items))))
           (car items))
          ((and (not (pair? (car items))) (pair? (cdr items)))
           (list (car items) (fringe (cdr items))))
          (else
            (append (fringe (car items)) (fringe (cdr items))))
          )))

; x = ((1 2) (3 4)) は分解できた
;
; (define z (list (list 1 (list 2 3) 4) (list (list (list 6 7 8) 9))))
; => ((1 (2 3) 4) (((6 7 8) 9)))
;
; これはできない。

; wikiの回答
(define (fringe x)
  (cond ((null? x) nil)
        ((pair? x) (append (fringe (car x)) (fringe (cdr x))))
        (else (list x))))

; ↑もzは分解できないからいいや。
