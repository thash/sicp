(add-load-path ".")
(load "my_defs")
(load "q2.38")

(define (reverse-r sequence)
  (fold-right
    (lambda (x y) (append y (list x)))
    ()
    sequence))
; gosh> (reverse-r (list 1 2 3))
; CALL accumulate #[proc] () (1 2 3)
;   CALL accumulate #[proc] () (2 3)
;     CALL accumulate #[proc] () (3)
;       CALL accumulate #[proc] () ()
;       RETN accumulate ()
;     RETN accumulate (3)
;   RETN accumulate (3 2)
; RETN accumulate (3 2 1)
; (3 2 1)
;
; TODO: appendを使わないパターン


(define (reverse-l sequence)
  (fold-left
    (lambda (x y) (cons y x))
    () sequence))
; gosh> (reverse-l (list 1 2 3))
; CALL iter () (1 2 3)
;   CALL iter (1) (2 3)
;     CALL iter (2 1) (3)
;       CALL iter (3 2 1) ()
;       RETN iter (3 2 1)
;     RETN iter (3 2 1)
;   RETN iter (3 2 1)
; RETN iter (3 2 1)
; (3 2 1)

