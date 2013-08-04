(add-load-path ".")
(add-load-path "./sec2")
(load "q2.18")

(define x (list (list 1 2) (list 3 4)))
;=> ((1 2) (3 4))

; gosh> (reverse x)
; ((3 4) (1 2))
; gosh> (deep-reverse x)
; ((4 3) (2 1))

(define (deep-reverse items)
  (if (null? items)
    ()
    (if (list? items)
      (append (deep-reverse (cdr items)) (cons (deep-reverse (car items)) ()))
      items
      )))

; appendの行をはじめ
; (append (deep-reverse (cdr items)) (deep-reverse (car items)))
; としていたが、エラー。#?=で見てみると
; !!!*** ERROR: list required, but got 4!!! となっていた。ので、nilとconsして強引にlistにする。


;; ((1 2) (3 4))
;; (3 4) (1 2)
;; (3) 4 (2) 1

; reverseはGauche組み込みであるので、こんな方法もある。これはきれいだ。
(define (deep-reverse2 items)
  (if (pair? items)
    (reverse (map deep-reverse2 items))
    items))
; mapでitemsそれぞれの要素に対してreverse. 対象itemsがpairじゃなくなった段階でitemを返す。
; もっとやるなら、carで一個先を見て再帰するとか。


;; 20130804 再帰部分がきれいになるよう注意して書きなおす.
(define (deep-reverse3 items)
  (if (null? items) '()
    (if (not (pair? items)) items
      (let ((next-first (if (null? (cdr items))
                            (cdr items) (car (cdr items))))
            (next-rest (car items)))
        (list (deep-reverse3 next-first)
              (deep-reverse3 next-rest))))))

