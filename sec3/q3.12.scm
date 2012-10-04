;; append!を作る。
;; xの最後のcdrをyにセットする。

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;; gosh> (cdr x)
;; (b)
;; gosh> z
;; (a b c d)

(define w (append! x y))
;; gosh> w
;; (a b c d)
;; gosh> (cdr x)
;; (b c d)
;;
;; !? ...箱とポインタの図を書け。


