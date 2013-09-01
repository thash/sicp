(car ''abracadabra)
; => quote

; 最初の'以降はすべてそのまま解釈される
; gosh> ''aaaa
; 'aaaa
; gosh> (cdr ''aaaa)
; (aaaa)

; 勉強会でメモ。
; 'はquote手続きの省略形である.
;     > Reader Syntax: 'datum
;     > [R5RS] (quote datum)の略記です。
;     > http://practical-scheme.net/gauche/man/gauche-refj_23.html

(quote (1 2 3 4))
; => (1 2 3 4)

; したがって''aaaはinterpreterにより
; (quote (quote aaa)) と解釈される。
(quote (quote abbb))
; => 'abbb
;; gosh> (car (quote (quote abcd)))
;; quote

;; 印字される形と実際のデータ型(?)が対応してない、ということ？ <- 違う
;; 最初のquoteを作用させた時点で2個目のquote手続きは手続きではなくデータとしてみなされる
;; つまり (quote abcd) というデータで, (x y) と同じように扱われるため,
;; (car '(x y)) が1であるように(car '(quote abcd)) はquoteとなる.

