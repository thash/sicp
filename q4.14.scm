(load "./sec4.1-The-Metacircular-Evaluator")

;; "Evaはmapの定義を入力し" => 素直なmapの再定義
;; つーかこの定義あってもなくても動く
(define (map f lis)
  (if (null? lis)
    '()
    (cons (f (car lis)) (map f (cdr lis)))))

; gosh> (map (lambda (x) (* x x)) '(1 2 3 4 5))
; (1 4 9 16 25)

;; で, またLouisか. Lousはmapをprimitive-procedureとして組み込んだそうな.

;; sec4.1-The-Metacircular-Evaluator.scm の
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'map map) ;; <= ここを加えた
        ))

;; どうもこいつはprimitive tagが最初についてるから動かないよ, と.

;; 言ってることはわかるが現象が再現できない.
;; そもそもこいつを反応しない...
; gosh> (primitive-procedure? 'car)
; #f
; gosh> (primitive-procedure? '(primitive null?))
; #t
;; こうか.


;; ふつーに動きますね.
;; gosh> (map (lambda (x) (* x x)) '(1 2 3 4 5))
;; (1 4 9 16 25)


