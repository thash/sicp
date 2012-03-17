(define hoge (list 1 3 (list 5 7) 9))
(define fuga (list (list 7)))
(define piyo (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; 7を取り出す。

(car (cdr (car (cdr (cdr hoge)))))
; or,
(car (cdaddr hoge)) ; cadaddrはなかった


(car (car fuga))
; or,
(caar fuga)


(car (cdr (cadr (cadr (cadr (cadr (cadr piyo)))))))
(car (cdr (cadr (cadr (cadr (cadadr piyo))))))
(car (cdr (cadr (cadadr (cadadr piyo)))))
(cadr (cadr (cadadr (cadadr piyo))))
(cadadr (cadadr (cadadr piyo)))

