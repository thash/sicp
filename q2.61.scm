; 場所を考えてconsしないといけない。
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; x > (car set) の時はどんどんcdrして進んでいく
; =になったら重複しないようにsetだけを返す。
; x < (car set) 、越えられない数が出てきたらそこでconsする

; gosh> (define s '(1 3 5))
; gosh> (adjoin-set 2 s)
; (1 2 3 5)
; gosh> (adjoin-set 9 s)
; (1 3 5 9)

