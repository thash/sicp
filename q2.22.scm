(add-load-path ".")
(load "my_defs")

; Q2.21(1) のsquare-listを反復プロセスに書き直したところ、逆順になってしまった。
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  ;(trace iter)
  (iter items ()))

(square-list (list 1 2 3 4 5))
;=> (25 16 9 4 1)

; 理由: carで最初の項から順に処理していき、
;       その結果を今までの結果が格納されているanswerの「前」に結合しているため
; check: trace iter
; CALL iter (1 2 3 4 5) ()
;   CALL iter (2 3 4 5) (1)
;     CALL iter (3 4 5) (4 1)
;       CALL iter (4 5) (9 4 1)
;         CALL iter (5) (16 9 4 1)
;         RETN iter (25 16 9 4 1)
;       RETN iter (25 16 9 4 1)
;     RETN iter (25 16 9 4 1)
;   RETN iter (25 16 9 4 1)
; RETN iter (25 16 9 4 1)


; さらにconsの前後を入れ替えても動かなかった。
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  ; (trace iter)
  (iter items ()))

(square-list2 (list 1 2 3 4 5))
;=> (((((( . 1) . 4) . 9) . 16) . 25))

; 理由: consによりリストを返すためには、引数が要素, リストの順である必要がある。
;        そうでなければ対が出来る
; gosh> (cons 1 ())
; (1)
; gosh> (cons () 1)
; (() . 1)
; gosh> (cons 1 2)
; (1 . 2)

; check: trace iter
; CALL iter (1 2 3 4 5) ()
;   CALL iter (2 3 4 5) (() . 1)
;     CALL iter (3 4 5) ((() . 1) . 4)
;       CALL iter (4 5) (((() . 1) . 4) . 9)
;         CALL iter (5) ((((...) . 4) . 9) . 16)
;         RETN iter ((((...) . 9) . 16) . 25)
;       RETN iter ((((...) . 9) . 16) . 25)
;     RETN iter ((((...) . 9) . 16) . 25)
;   RETN iter ((((...) . 9) . 16) . 25)
; RETN iter ((((...) . 9) . 16) . 25)


