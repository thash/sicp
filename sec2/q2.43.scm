(add-load-path ".")
(load "q2.42")

; slow version of queens: Louisが間違って書いていた遅い版.
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
                 (queen-cols (- k 1)))) ;; <- queen-colsと
          (enumerate-interval 1 board-size))))) ;; <- enumerate-intervalが, 逆になってる.
  (queen-cols board-size))

;; まずは計測.
;; q2.42 で定義したものをそのまま使うとsize6で0.001秒.
;    gosh> (time (queens 6))
;    !!!;(time (queens 6))!!!
;    !!!; real   0.001!!!
;    !!!; user   0.000!!!
;    !!!; sys    0.000!!!

;; 一方slow-queensは0.150秒. 100倍かかってる.
;    gosh> (time (slow-queens 6))
;    !!!;(time (slow-queens 6))!!!
;    !!!; real   0.142!!!
;    !!!; user   0.150!!!
;    !!!; sys    0.000!!!
;;  sizeを10にすると1分以上かかるぽい(待ってない).

;; 問: なぜ遅いか.
;; queen-colsを無駄に呼び出している.
;; (1 2 3 4) に対して本来1回mapすれば良い所を, 4回やってる.

;; 問: 2.42の解答をTとするとどう表せるか.
;; board-sizeのべき乗で増加することになるので
;; T*(board-size^board-size) のオーダーっぽい


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

