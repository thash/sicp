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

; elseの中で二つの式を評価させるためにifでは都合が悪かった

