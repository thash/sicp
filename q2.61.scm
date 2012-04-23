; 場所を考えてconsしないといけない。
(load "./my_defs")
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; (trace adjoin-set)

; x > (car set) の時はどんどんcdrして進んでいく
; =になったら重複しないようにsetだけを返す。
; x < (car set) 、越えられない数が出てきたらそこでconsする

; gosh> (define s '(1 3 5))
; gosh> (adjoin-set 2 s)
; (1 2 3 5)
; gosh> (adjoin-set 9 s)
; (1 3 5 9)

; gosh> (adjoin-set 2 s)
; CALL adjoin-set 2 (1 3 5)
;   CALL adjoin-set 2 (3 5)
;   RETN adjoin-set (2 3 5)
; RETN adjoin-set (1 2 3 5)
; (1 2 3 5)
; gosh> (adjoin-set 9 s)
; CALL adjoin-set 9 (1 3 5)
;   CALL adjoin-set 9 (3 5)
;     CALL adjoin-set 9 (5)
;       CALL adjoin-set 9 ()
;       RETN adjoin-set (9)
;     RETN adjoin-set (5 9)
;   RETN adjoin-set (3 5 9)
; RETN adjoin-set (1 3 5 9)
; (1 3 5 9)
; gosh> (adjoin-set 3 s)
; CALL adjoin-set 3 (1 3 5)
;   CALL adjoin-set 3 (3 5)
;   RETN adjoin-set (3 5)
; RETN adjoin-set (1 3 5)
; (1 3 5)
