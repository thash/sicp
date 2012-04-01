(add-load-path ".")
(load "my_defs")

; accumulate 再定義
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(trace accumulate)

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (trace iter)
  (iter initial sequence))


; gosh> (fold-right / 1 (list 1 2 3))
; gosh> CALL accumulate #[proc] 1 (1 2 3)
;   CALL accumulate #[proc] 1 (2 3)
;     CALL accumulate #[proc] 1 (3)
;       CALL accumulate #[proc] 1 ()
;       RETN accumulate 1
;     RETN accumulate 3
;   RETN accumulate 2/3
; RETN accumulate 3/2
; 3/2

; gosh> (fold-left / 1 (list 1 2 3))
; CALL iter 1 (1 2 3)
;   CALL iter 1 (2 3)
;     CALL iter 1/2 (3)
;       CALL iter 1/6 ()
;       RETN iter 1/6
;     RETN iter 1/6
;   RETN iter 1/6
; RETN iter 1/6
; 1/6

; gosh> (fold-right list () (list 1 2 3))
; CALL accumulate #[proc] () (1 2 3)
;   CALL accumulate #[proc] () (2 3)
;     CALL accumulate #[proc] () (3)
;       CALL accumulate #[proc] () ()
;       RETN accumulate ()
;     RETN accumulate (3 ())
;   RETN accumulate (2 (3 ()))
; RETN accumulate (1 (2 (3 ())))
; (1 (2 (3 ())))

; gosh> (fold-left list () (list 1 2 3))
; CALL iter () (1 2 3)
;   CALL iter (() 1) (2 3)
;     CALL iter ((() 1) 2) (3)
;       CALL iter (((...) ...) 3) ()
;       RETN iter (((() ...) 2) 3)
;     RETN iter (((() ...) 2) 3)
;   RETN iter (((() ...) 2) 3)
; RETN iter (((() ...) 2) 3)
; (((() 1) 2) 3)


; 上のようにfold-leftとfold-rightはop, initial, sequenceの引数が同じでも異なる結果を返す。
; 同じ結果を返すためにopが満たすべき条件は、(op a b)と(op b a)の結果が等しいような手続きであること。
; "commutative:交換可能な"というらしい。
; 割り算もlistも項を代えると結果も違うタイプの演算。








