(car ''abracadabra)
; => quote

; 最初の'以降はすべてそのまま解釈される
; gosh> ''aaaa
; 'aaaa
; gosh> (cdr ''aaaa)
; (aaaa)

; 勉強会でメモ。
; 'はquote手続きの省略形(?)である。
(quote (1 2 3 4))
; => (1 2 3 4)

; したがって''aaaはinterpreterにより
; (quote (quote aaa)) と解釈される。
(quote (quote abbb))
; => 'abbb

; 印字される形と実際のデータ型(?)が対応してない、ということ？


