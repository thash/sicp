;; ちゃんとorder順になるようにconsしないといけない.
(add-load-path "." :relative)
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

    (adjoin-set 2 '(1 3 5))
    ;; => CALL adjoin-set 2 (1 3 5)
    ;;      CALL adjoin-set 2 (3 5)
    ;;      RETN adjoin-set (2 3 5)
    ;;    RETN adjoin-set (1 2 3 5)
    ;;    (1 2 3 5)

    (adjoin-set 3 '(1 3 5))
    ;; => CALL adjoin-set 3 (1 3 5)
    ;;      CALL adjoin-set 3 (3 5)
    ;;      RETN adjoin-set (3 5)
    ;;    RETN adjoin-set (1 3 5)
    ;;    (1 3 5)

