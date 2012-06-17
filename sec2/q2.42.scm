(add-load-path ".")
(load "my_defs")
(load "sec2.2.3") ; load filter, enumerate-interval
(load "sec2.2.3-nesting") ; load flatmap
; eight-queens puzzle
; (queens n) と使えばnxnのchess baordにおけるすべての回答を返すような手続きqueensを定義したい。

; require "filter", "flatmap", "enumerate-interval"
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board) ; empty-board = ?
      (filter
        (lambda (positions) (safe? k positions)) ; safe? = ?
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens)) ; adjoin-position = ?
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; 概要は示された。ここで未定義状態の
;   * empty-board
;   * safe?
;   * adjoin-position
; を定義せよ。

(define empty-board ())

(define (adjoin-position new-row column rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? column positions)
  (define (next-column-safe? new-row positions row-offset)
    (if (null? positions)
      #t
      (let ((this-row (car positions)))
        (if (or (= this-row new-row)
                (= (+ this-row row-offset) new-row)
                (= (- this-row row-offset) new-row))
          #f
          (next-column-safe? new-row (cdr positions) (+ 1 row-offset))))))
  (next-column-safe? (car positions) (cdr positions) 1))

; 引数っぽいのは1..board-size. まずboard-size = 4.
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens)) ; adjoin-position = ?
;                 (enumerate-interval 1 board-size)))
; new-rowには(1 2 3 4)が順に入ってくる
;
; 基本的な考え方 by @haruyamaさん
; filterの前で全ての可能性を洗い出す。
; 1列目の1におかれているとき, 2個目をおいたときの可能性は (1 1) (1 2) (1 3) (1 4)
; そいつをfilterにかけて(1 3) (1 4)がのこる。これを続けていく。
;
; アルゴリズムについて by あんちべさん
; すべての可能性をあたると64C4で4億を超えるとか(?)。
; バックトラック法、という方法を使う。
; 1列に1個だけおける、という前提自体が「同じ行におくと即死するから」である。
;
;
; 肝心の safe? で何をしているか。
; safe?は引数にcolumnとpositionsを取る。
;
; (if (or (= this-row new-row)
;         (= (+ this-row row-offset) new-row)
;         (= (- this-row row-offset) new-row))
;
; rowが同じだったら飛車で死
;


; queens 1,2,3には解がない。()が帰る。
; gosh> (queens 4) {{{
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 2 (1 1)
; RETN safe? #f
; CALL safe? 2 (2 1)
; RETN safe? #f
; CALL safe? 2 (3 1)
; RETN safe? #t
; CALL safe? 2 (4 1)
; RETN safe? #t
; CALL safe? 2 (1 2)
; RETN safe? #f
; CALL safe? 2 (2 2)
; RETN safe? #f
; CALL safe? 2 (3 2)
; RETN safe? #f
; CALL safe? 2 (4 2)
; RETN safe? #t
; CALL safe? 2 (1 3)
; RETN safe? #t
; CALL safe? 2 (2 3)
; RETN safe? #f
; CALL safe? 2 (3 3)
; RETN safe? #f
; CALL safe? 2 (4 3)
; RETN safe? #f
; CALL safe? 2 (1 4)
; RETN safe? #t
; CALL safe? 2 (2 4)
; RETN safe? #t
; CALL safe? 2 (3 4)
; RETN safe? #f
; CALL safe? 2 (4 4)
; RETN safe? #f
; CALL safe? 3 (1 3 1)
; RETN safe? #f
; CALL safe? 3 (2 3 1)
; RETN safe? #f
; CALL safe? 3 (3 3 1)
; RETN safe? #f
; CALL safe? 3 (4 3 1)
; RETN safe? #f
; CALL safe? 3 (1 4 1)
; RETN safe? #f
; CALL safe? 3 (2 4 1)
; RETN safe? #t
; CALL safe? 3 (3 4 1)
; RETN safe? #f
; CALL safe? 3 (4 4 1)
; RETN safe? #f
; CALL safe? 3 (1 4 2)
; RETN safe? #t
; CALL safe? 3 (2 4 2)
; RETN safe? #f
; CALL safe? 3 (3 4 2)
; RETN safe? #f
; CALL safe? 3 (4 4 2)
; RETN safe? #f
; CALL safe? 3 (1 1 3)
; RETN safe? #f
; CALL safe? 3 (2 1 3)
; RETN safe? #f
; CALL safe? 3 (3 1 3)
; RETN safe? #f
; CALL safe? 3 (4 1 3)
; RETN safe? #t
; CALL safe? 3 (1 1 4)
; RETN safe? #f
; CALL safe? 3 (2 1 4)
; RETN safe? #f
; CALL safe? 3 (3 1 4)
; RETN safe? #t
; CALL safe? 3 (4 1 4)
; RETN safe? #f
; CALL safe? 3 (1 2 4)
; RETN safe? #f
; CALL safe? 3 (2 2 4)
; RETN safe? #f
; CALL safe? 3 (3 2 4)
; RETN safe? #f
; CALL safe? 3 (4 2 4)
; RETN safe? #f
; CALL safe? 4 (1 2 4 1)
; RETN safe? #f
; CALL safe? 4 (2 2 4 1)
; RETN safe? #f
; CALL safe? 4 (3 2 4 1)
; RETN safe? #f
; CALL safe? 4 (4 2 4 1)
; RETN safe? #f
; CALL safe? 4 (1 1 4 2)
; RETN safe? #f
; CALL safe? 4 (2 1 4 2)
; RETN safe? #f
; CALL safe? 4 (3 1 4 2)
; RETN safe? #t
; CALL safe? 4 (4 1 4 2)
; RETN safe? #f
; CALL safe? 4 (1 4 1 3)
; RETN safe? #f
; CALL safe? 4 (2 4 1 3)
; RETN safe? #t
; CALL safe? 4 (3 4 1 3)
; RETN safe? #f
; CALL safe? 4 (4 4 1 3)
; RETN safe? #f
; CALL safe? 4 (1 3 1 4)
; RETN safe? #f
; CALL safe? 4 (2 3 1 4)
; RETN safe? #f
; CALL safe? 4 (3 3 1 4)
; RETN safe? #f
; CALL safe? 4 (4 3 1 4)
; RETN safe? #f
; ((3 1 4 2) (2 4 1 3)) }}}

