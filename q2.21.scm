(add-load-path ".")
(load "sec2.2")


; "数のリストを引数に取り、各要素を2乗したリストを返すsquare-list"

(define (square x) (* x x))

; (1) 再帰的プロセス
(define (square-list items)
  (if (null? items)
    ()
    (cons (* (car items) (car items)) (square-list (cdr items)))))

; もしくは (cons (square (car items)) (square-list (cdr items)))))


; (2) mapを使う解法
(define (square-list2 items)
(map (lambda (x) (* x x)) items))

; もしくは (map square items))

