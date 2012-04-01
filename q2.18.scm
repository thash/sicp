(add-load-path ".")
(load "my_defs")
; 内部iterでresult にリストを持ちつつconsで合成していく
(define (reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items)  result))))
  (trace iter)
  (iter items ()))

; 最初consで対にする順番を逆にしてこうなった。 => (((((( . 1) . 2) . 3) . 4) . 5)
