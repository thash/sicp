; for-each: 引数として手続きと要素のリストを作るところはmapと同じだが、
;           結果としてリストを返すのではなく左から右へ順に手続きを作用させるだけ。

; 使い方 (返り値は何でもいい)
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(define (for-each proc items)
  (cond ((null? items) #t)
    (else
     (proc (car items))
     (for-each proc (cdr items))
     )))

; elseの中で二つの式を評価させるためにifでは都合が悪かった。
; webで探してcondを使うと良いらしいと知る。あと(and (式x) (式y))とかやる方法も。

;;; 解説議論 ;;;
; というか、返り値を気にせず評価させればいいなら方法はいろいろあるらしい。たとえばconsでもいい。
(define (for-each proc items)
  (if (null? items) #t
    (cons (proc (car items)) (for-each proc (cdr items)))))

; 正式な(?)方法としては、beginを使って2つ以上評価させるらしい。
(define (for-each proc items)
  (if (null? items) #t
    (begin (proc (car items)) (for-each proc (cdr items)))))
