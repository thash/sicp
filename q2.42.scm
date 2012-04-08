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

