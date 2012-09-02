(load "./stream")

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; gosh> (display-stream-n (expand 1 7 10) 20)
; 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, done
; gosh> (display-stream-n (expand 2 5 10) 10)
; 4, 0, 0, 0, 0, 0, 0, 0, 0, done
; gosh> (display-stream-n (expand 2 6 10) 10)
; 3, 3, 3, 3, 3, 3, 3, 3, 3, done
; gosh> (display-stream-n (expand 13 2 10) 10)
; 65, 0, 0, 0, 0, 0, 0, 0, 0, done

;; num/denを計算したときの小数点以下の数字列が表示される。
;; radixは奇数. 2にすると2進数扱い


