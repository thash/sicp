(add-load-path ".")
(load "my_defs")

; matrix
; | 1 2 3 4 |
; | 4 5 6 6 |
; | 6 7 8 9 |
; => ((1 2 3 4) (4 5 6 6) (6 7 8 9)) と表現

; accumulate, accumulate-n
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs)) ; 一個下を見なければならない
    ()
    (cons (accumulate op initial (map car seqs))
          (accumulate-n op initial (map cdr seqs)))))
(trace accumulate)
(trace accumulate-n)


(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 9 8 7))
(define e3 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define e4 (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

; vector同士の"内積"
(define (dot-product v w)
  (accumulate + 0 (map * v w))) ; (map proc list1 list2) yet another map
;; gosh> (map * (list 1 2 3) (list 3 4 5))
;; (3 8 15)

; gosh> (dot-product v v) {{{
; CALL accumulate #[proc] 0 (81 64 49)
;   CALL accumulate #[proc] 0 (64 49)
;     CALL accumulate #[proc] 0 (49)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 49
;   RETN accumulate 113
; RETN accumulate 194
; 194 }}}


(define (matrix-*-vector m v)
  (map
    (lambda (row) (dot-product row v))
    m))
; return the vector. t(i) = SUMj{m(ij)*v(j)}
; mの各要素(=vector=list)がrowとしてvとdot-productされる。

;; 本文と別の例:
;; gosh> (matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 1 2 3))
;; (14 32 50)

; gosh> (matrix-*-vector m v) {{{
; CALL dot-product (1 2 3 4) (9 8 7)
;   CALL accumulate #[proc] 0 (9 16 21)
;     CALL accumulate #[proc] 0 (16 21)
;       CALL accumulate #[proc] 0 (21)
;         CALL accumulate #[proc] 0 ()
;         RETN accumulate 0
;       RETN accumulate 21
;     RETN accumulate 37
;   RETN accumulate 46
; RETN dot-product 46
; CALL dot-product (4 5 6 6) (9 8 7)
;   CALL accumulate #[proc] 0 (36 40 42)
;     CALL accumulate #[proc] 0 (40 42)
;       CALL accumulate #[proc] 0 (42)
;         CALL accumulate #[proc] 0 ()
;         RETN accumulate 0
;       RETN accumulate 42
;     RETN accumulate 82
;   RETN accumulate 118
; RETN dot-product 118
; CALL dot-product (6 7 8 9) (9 8 7)
;   CALL accumulate #[proc] 0 (54 56 56)
;     CALL accumulate #[proc] 0 (56 56)
;       CALL accumulate #[proc] 0 (56)
;         CALL accumulate #[proc] 0 ()
;         RETN accumulate 0
;       RETN accumulate 56
;     RETN accumulate 112
;   RETN accumulate 166
; RETN dot-product 166
; (46 118 166) }}}

(define (transpose m)
  (accumulate-n cons () m))

; gosh> (transpose m) {{{
; CALL accumulate-n #[proc] () ((1 2 ...) (4 5 6 6) (6 7 8 9))
;   CALL accumulate #[proc] () (1 4 6)
;     CALL accumulate #[proc] () (4 6)
;       CALL accumulate #[proc] () (6)
;         CALL accumulate #[proc] () ()
;         RETN accumulate ()
;       RETN accumulate (6)
;     RETN accumulate (4 6)
;   RETN accumulate (1 4 6)
;   CALL accumulate-n #[proc] () ((2 3 ...) (5 6 6) (7 8 9))
;     CALL accumulate #[proc] () (2 5 7)
;       CALL accumulate #[proc] () (5 7)
;         CALL accumulate #[proc] () (7)
;           CALL accumulate #[proc] () ()
;           RETN accumulate ()
;         RETN accumulate (7)
;       RETN accumulate (5 7)
;     RETN accumulate (2 5 7)
;     CALL accumulate-n #[proc] () ((3 4) (6 6) (8 9))
;       CALL accumulate #[proc] () (3 6 8)
;         CALL accumulate #[proc] () (6 8)
;           CALL accumulate #[proc] () (8)
;             CALL accumulate #[proc] () ()
;             RETN accumulate ()
;           RETN accumulate (8)
;         RETN accumulate (6 8)
;       RETN accumulate (3 6 8)
;       CALL accumulate-n #[proc] () ((4) (6) (9))
;         CALL accumulate #[proc] () (4 6 9)
;           CALL accumulate #[proc] () (6 9)
;             CALL accumulate #[proc] () (9)
;               CALL accumulate #[proc] () ()
;               RETN accumulate ()
;             RETN accumulate (9)
;           RETN accumulate (6 9)
;         RETN accumulate (4 6 9)
;         CALL accumulate-n #[proc] () (() () ())
;         RETN accumulate-n ()
;       RETN accumulate-n ((4 6 9))
;     RETN accumulate-n ((3 6 8) (4 6 9))
;   RETN accumulate-n ((2 5 7) (3 6 8) (4 6 9))
; RETN accumulate-n ((1 ...) (2 5 7) (3 6 8) (4 6 9))
; ((1 4 6) (2 5 7) (3 6 8) (4 6 9)) }}}

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (transpose (map
                 (lambda (v) (matrix-*-vector m v))
                 cols))))

(define m1 (list (list 1 2 3) (list 3 5 1) (list 1 1 1)))
(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; gosh> (matrix-*-matrix m1 m2) {{{
; CALL accumulate-n #[proc] () ((1 2 ...) (4 5 6) (7 8 9))
;   CALL accumulate #[proc] () (1 4 7)
;     CALL accumulate #[proc] () (4 7)
;       CALL accumulate #[proc] () (7)
;         CALL accumulate #[proc] () ()
;         RETN accumulate ()
;       RETN accumulate (7)
;     RETN accumulate (4 7)
;   RETN accumulate (1 4 7)
;   CALL accumulate-n #[proc] () ((2 3) (5 6) (8 9))
;     CALL accumulate #[proc] () (2 5 8)
;       CALL accumulate #[proc] () (5 8)
;         CALL accumulate #[proc] () (8)
;           CALL accumulate #[proc] () ()
;           RETN accumulate ()
;         RETN accumulate (8)
;       RETN accumulate (5 8)
;     RETN accumulate (2 5 8)
;     CALL accumulate-n #[proc] () ((3) (6) (9))
;       CALL accumulate #[proc] () (3 6 9)
;         CALL accumulate #[proc] () (6 9)
;           CALL accumulate #[proc] () (9)
;             CALL accumulate #[proc] () ()
;             RETN accumulate ()
;           RETN accumulate (9)
;         RETN accumulate (6 9)
;       RETN accumulate (3 6 9)
;       CALL accumulate-n #[proc] () (() () ())
;       RETN accumulate-n ()
;     RETN accumulate-n ((3 6 9))
;   RETN accumulate-n ((2 5 8) (3 6 9))
; RETN accumulate-n ((1 4 7) (2 5 8) (3 6 9))
; CALL accumulate #[proc] 0 (1 8 21)
;   CALL accumulate #[proc] 0 (8 21)
;     CALL accumulate #[proc] 0 (21)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 21
;   RETN accumulate 29
; RETN accumulate 30
; CALL accumulate #[proc] 0 (3 20 7)
;   CALL accumulate #[proc] 0 (20 7)
;     CALL accumulate #[proc] 0 (7)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 7
;   RETN accumulate 27
; RETN accumulate 30
; CALL accumulate #[proc] 0 (1 4 7)
;   CALL accumulate #[proc] 0 (4 7)
;     CALL accumulate #[proc] 0 (7)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 7
;   RETN accumulate 11
; RETN accumulate 12
; CALL accumulate #[proc] 0 (2 10 24)
;   CALL accumulate #[proc] 0 (10 24)
;     CALL accumulate #[proc] 0 (24)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 24
;   RETN accumulate 34
; RETN accumulate 36
; CALL accumulate #[proc] 0 (6 25 8)
;   CALL accumulate #[proc] 0 (25 8)
;     CALL accumulate #[proc] 0 (8)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 8
;   RETN accumulate 33
; RETN accumulate 39
; CALL accumulate #[proc] 0 (2 5 8)
;   CALL accumulate #[proc] 0 (5 8)
;     CALL accumulate #[proc] 0 (8)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 8
;   RETN accumulate 13
; RETN accumulate 15
; CALL accumulate #[proc] 0 (3 12 27)
;   CALL accumulate #[proc] 0 (12 27)
;     CALL accumulate #[proc] 0 (27)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 27
;   RETN accumulate 39
; RETN accumulate 42
; CALL accumulate #[proc] 0 (9 30 9)
;   CALL accumulate #[proc] 0 (30 9)
;     CALL accumulate #[proc] 0 (9)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 9
;   RETN accumulate 39
; RETN accumulate 48
; CALL accumulate #[proc] 0 (3 6 9)
;   CALL accumulate #[proc] 0 (6 9)
;     CALL accumulate #[proc] 0 (9)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 9
;   RETN accumulate 15
; RETN accumulate 18
; CALL accumulate-n #[proc] () ((30 30 ...) (36 39 ...) (42 48 18))
;   CALL accumulate #[proc] () (30 36 42)
;     CALL accumulate #[proc] () (36 42)
;       CALL accumulate #[proc] () (42)
;         CALL accumulate #[proc] () ()
;         RETN accumulate ()
;       RETN accumulate (42)
;     RETN accumulate (36 42)
;   RETN accumulate (30 36 42)
;   CALL accumulate-n #[proc] () ((30 12) (39 15) (48 18))
;     CALL accumulate #[proc] () (30 39 48)
;       CALL accumulate #[proc] () (39 48)
;         CALL accumulate #[proc] () (48)
;           CALL accumulate #[proc] () ()
;           RETN accumulate ()
;         RETN accumulate (48)
;       RETN accumulate (39 48)
;     RETN accumulate (30 39 48)
;     CALL accumulate-n #[proc] () ((12) (15) (18))
;       CALL accumulate #[proc] () (12 15 18)
;         CALL accumulate #[proc] () (15 18)
;           CALL accumulate #[proc] () (18)
;             CALL accumulate #[proc] () ()
;             RETN accumulate ()
;           RETN accumulate (18)
;         RETN accumulate (15 18)
;       RETN accumulate (12 15 18)
;       CALL accumulate-n #[proc] () (() () ())
;       RETN accumulate-n ()
;     RETN accumulate-n ((12 15 18))
;   RETN accumulate-n ((30 39 48) (12 15 18))
; RETN accumulate-n ((30 36 42) (30 39 48) (12 15 18))
; ((30 36 42) (30 39 48) (12 15 18)) }}}

;; 別の例: 単位行列かけても変化なし
;; gosh> (matrix-*-matrix m1 e3)
;; ((1 2 3) (3 5 1) (1 1 1))

; ---------------------------------------
;
; ##別の例
;
; * matrix x vector
;
; | 1 2 3 |   | 1 |   | 14 |
; | 4 5 6 | x | 2 | = | 32 |
; | 7 8 9 |   | 3 |   | 50 |
;
; gosh> (matrix-*-vector m v) {{{
; CALL accumulate #[proc] 0 (1 4 9)
;   CALL accumulate #[proc] 0 (4 9)
;     CALL accumulate #[proc] 0 (9)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 9
;   RETN accumulate 13
; RETN accumulate 14
; CALL accumulate #[proc] 0 (4 10 18)
;   CALL accumulate #[proc] 0 (10 18)
;     CALL accumulate #[proc] 0 (18)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 18
;   RETN accumulate 28
; RETN accumulate 32
; CALL accumulate #[proc] 0 (7 16 27)
;   CALL accumulate #[proc] 0 (16 27)
;     CALL accumulate #[proc] 0 (27)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;     RETN accumulate 27
;   RETN accumulate 43
; RETN accumulate 50
; (14 32 50) }}}


