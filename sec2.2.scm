(add-load-path ".")
(load "my_defs")

;-- 2.2 --;
; consで作る「対」は「のり」の役割を果たす。
; 次のページに「箱とポインタ」記法
; 要素が対であるような対を作ることが出来る（閉包性, closure property）。階層構造を表せる。

;-- 2.2.1 --;
; 対を使って便利な構造「並び(sequence)」を作る。図2.4。それぞれcarが項、cdrが次の対（最後はnil）。

(define seq
(cons 1
      (cons 2
            (cons 3
                  (cons 4 ())))) ; Gaucheではnilは()
)

; sequenceはlist手続きによっても作ることができる。上と等価。
(list 1 2 3 4)

; (list 1 2 3 4) は手続きlistを1,2,3,4に適用する「式」で、
; (1 2 3 4) はリスト。↓を評価するとエラーになる。
(1 2 3 4)

; carは最初の項、cdrは最初の項を除いた残りのリストを返す。
(define one-through-four (list 1 2 3 4))
(car one-through-four)
(cdr one-through-four)

; consでさらに組み合わせる。
(cons 5 one-through-four)
(cons one-through-four 5) ; => (1 2 3 4 5) にならない?


; リスト演算
; リストを"cdrダウン"することで順々に要素を扱う
; list-refはlistのn個目を取得する手続き
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

; リストが空になるまでcdrダウンする。例: listの要素数を求める
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length (list 1 2 3 4 5))

; 反復バージョンlength
(define (length2 items)
  (define (iter items count)
  (if (null? items)
    count
    (iter (cdr items) (+ count 1))))
  (iter items 0))

; 二つのリストを結合する手続きappend。"cdrダウン"しつつ"consアップ"する。
; list1をcarで頭から分解し、list2に結果をまとめていく。
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(append squares odds)
(append odds squares)
