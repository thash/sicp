; 2.3.3. 例: 集合の表現
; 集合をに使われる以下の演算を定義する。
; 逆に言うとこれらの規則が成り立つものが集合である。
; union-set, intersection-set, element-of-set?, adjoin-set
; それぞれ和集合, 積集合, 集合の要素かどうか, 集合に要素を追加

; element-of-set?はmemqに似ている。
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if ((element-of-set? x set) set)
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
           (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; gosh> (define set (list 1 2 3))
; gosh> (element-of-set? 1 set)
; #t
; gosh> (element-of-set? 5 set)
; #f
; gosh> (define setx (list 1 3 5 7 9))
; gosh> (intersection-set set setx)
; (1 3)

; element-of-set?はすべての手続きに使われるため効率が重要になる。
; この実装ではすべての集合を走査しており非効率である。最悪の場合、全部舐めた後に含まれないことがわかる。
; 要素数nに対してnステップを必要とするため、O(n)の計算量増加である。
; intersection-setになるとset1の各要素についてelement-of-set?で舐めるためn x n, O(n^2)である。

; union-setの実装はq2.59へ。
