(add-load-path ".")
(load "my_defs")
(load "./sec2/sec2.2.3") ;; load filter, flatmap, and enumerate-interval.

; eight-queens puzzle
; (queens n) と使えばnxnのchess baordにおけるすべての回答を返すような手続きqueensを定義したい。
; 問題文に以下の様なqueensの全体像が示されている
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board) ; <- empty-board (undefined)
      (filter
        (lambda (positions) (safe? k positions)) ; <- safe? (undefined)
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens)) ; <- adjoin-position (undefined)
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; * 内部手続きqueen-colsをk,k-1,...,0と再帰的に適用(k=0で終了)
;; * filterの中が処理本体.

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
    ;; displays are added for debug
;;    (display (cond ((= row-offset 1) "")
;;                   ((= row-offset 2) "    ")
;;                   ((= row-offset 3) "        ")
;;                   ((= row-offset 4) "            ")))
;;    (display #`"(next-column-safe? ,new-row ,positions ,row-offset)\n")
    (if (null? positions)
      #t
      (let ((this-row (car positions)))
        (if (or (= this-row new-row)
                (= (+ this-row row-offset) new-row)
                (= (- this-row row-offset) new-row))
          #f
          (next-column-safe? new-row (cdr positions) (+ 1 row-offset))))))
;;  (display #`"---------------------\n(safe? ,column ,positions)\n")
  (next-column-safe? (car positions) (cdr positions) 1))

; 引数っぽいのは1..board-size. まずboard-size = 4. (1,2,3 には解がなく()が返る)
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens)) ; adjoin-position = ?
;                 (enumerate-interval 1 board-size)))
; new-rowには, enumerate-intervalで組み立てた(1 2 3 4)が順に入ってくる
;
; 基本的な考え方「filterの"前"で全ての可能性を洗い出す」 by @haruyamaさん
; 例えば1列目の1におかれているとき, 2個目をおいたときあり得る状態は (1 1) (1 2) (1 3) (1 4)
; そいつをfilterにかけて(1 3) (1 4)がのこる。これを続けていく。
;
; アルゴリズムについて by あんちべさん
; すべての可能性をあたると64C4で4億を超えるとか?
; (64C4を計算したら635,376パターンだったので聞き間違えたか)
; バックトラック法、という方法を使う。
; 1列に1個だけおける、という前提自体が「同じ行におくと即死するから」である。
;
;
; 肝心の safe? で具体的に何をしているか.
;
; (if (or (= this-row new-row)
;         (= (+ this-row row-offset) new-row)
;         (= (- this-row row-offset) new-row))
;
; safe?は引数にcolumnとpositionsを取る.

;; sec2/q2.42-review.scm にメモを残しているのでこちらも参照.
