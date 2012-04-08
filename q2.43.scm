(add-load-path ".")
(load "q2.42")
; slow version of queens
; require "empty-board", "adjoin-position", "safe?"
(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; (trace safe?)
; gosh> (slow-queens 4) {{{
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
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
; CALL safe? 2 (1 2)
; RETN safe? #f
; CALL safe? 2 (1 3)
; RETN safe? #t
; CALL safe? 2 (1 4)
; RETN safe? #t
; CALL safe? 2 (2 1)
; RETN safe? #f
; CALL safe? 2 (2 2)
; RETN safe? #f
; CALL safe? 2 (2 3)
; RETN safe? #f
; CALL safe? 2 (2 4)
; RETN safe? #t
; CALL safe? 2 (3 1)
; RETN safe? #t
; CALL safe? 2 (3 2)
; RETN safe? #f
; CALL safe? 2 (3 3)
; RETN safe? #f
; CALL safe? 2 (3 4)
; RETN safe? #f
; CALL safe? 2 (4 1)
; RETN safe? #t
; CALL safe? 2 (4 2)
; RETN safe? #t
; CALL safe? 2 (4 3)
; RETN safe? #f
; CALL safe? 2 (4 4)
; RETN safe? #f
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
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
; CALL safe? 2 (1 2)
; RETN safe? #f
; CALL safe? 2 (1 3)
; RETN safe? #t
; CALL safe? 2 (1 4)
; RETN safe? #t
; CALL safe? 2 (2 1)
; RETN safe? #f
; CALL safe? 2 (2 2)
; RETN safe? #f
; CALL safe? 2 (2 3)
; RETN safe? #f
; CALL safe? 2 (2 4)
; RETN safe? #t
; CALL safe? 2 (3 1)
; RETN safe? #t
; CALL safe? 2 (3 2)
; RETN safe? #f
; CALL safe? 2 (3 3)
; RETN safe? #f
; CALL safe? 2 (3 4)
; RETN safe? #f
; CALL safe? 2 (4 1)
; RETN safe? #t
; CALL safe? 2 (4 2)
; RETN safe? #t
; CALL safe? 2 (4 3)
; RETN safe? #f
; CALL safe? 2 (4 4)
; RETN safe? #f
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
; CALL safe? 1 (1)
; RETN safe? #t
; CALL safe? 1 (2)
; RETN safe? #t
; CALL safe? 1 (3)
; RETN safe? #t
; CALL safe? 1 (4)
; RETN safe? #t
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
; ..... }}}

