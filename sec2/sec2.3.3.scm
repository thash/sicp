;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.3. 例: 集合の表現 -- p.89
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 集合に使われる以下の演算を定義する。
; 逆に言うとこれらの規則が成り立つものが集合である。
; union-set, intersection-set, element-of-set?, adjoin-set
; それぞれ和集合, 積集合, 集合の要素かどうか, 集合に要素を追加

; element-of-set?はmemqに似ている。
;; 要素が記号でなくても良いようにeq?の代わりにequal?を使う.
;; (前の章でequal?を定義したが, Gauche組み込みのものを使う.)
;; 効率について **1 で言及.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

    (element-of-set? 'a '(a b c)) ;; => #t
    (element-of-set? 'z '(a b c)) ;; => #f


(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set)))

    (adjoin-set 'y '(a b c)) ;; => (y a b c)
    (adjoin-set 'c '(a b c)) ;; => (a b c)


;; 集合の積
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
           (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

    (intersection-set '(a b c) '(b c)) ;; => (b c)
    (intersection-set '(a b c) '(d e)) ;; => ()


;; **1: element-of-set?の効率について
; element-of-set?はすべての手続きに使われるため効率が重要になる。
; この実装ではすべての集合を走査しており非効率である。最悪の場合、全部舐めた後に含まれないことがわかる。
; 要素数nに対してnステップを必要とするため、O(n)の計算量増加である。
; intersection-setになるとset1の各要素についてelement-of-set?で舐めるためn x n, O(n^2)である。
;; リストに何も付属情報/情報を与えるデータ構造がないので片端から走査するしかないのかな

;; => q2.59: union-set(集合の和)の実装
;; => q2.60: 重複を許すバージョンの実装


;; 順序づけられたリストとしての集合
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 順序づけることでelement-of-set?で全要素を走査する必要がなくなり効率up.

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

;; 数の大きさを見て途中で中断できる. 走査するために平均してn/2のステップ数が必要となるため,
;; オーダーは変わらずO(n)であるが, 2倍の改善が見込める.

;; intersection-setはO(n^2) -> O(n)になり素晴らしい.
;; 当初のverではsec1の個数 x sec2の個数分の比較が必要だったが,
;; 走査対象もsortされてるので一緒に減らしていける.
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
    (let ((x1 (car set1)) (x2 (car se2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2)))) ; 走査対象set2も縮めていくことができる
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2))) ; set2のcar要素、次の奴が取れるようにする
            ))))
;; set1 and/or set2 の最初の要素を除去して、より小さな集合の問題に引き下げている。

;; => q2.61: 順序ありsetにおけるadjoin-setを作れ.
;; => q2.61: 順序ありsetにおけるunion-setをO(n)で作れ.


;; 二進木としての集合
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; > 木はリストを使って表現出来る.
;; 各節は三つの項のリストである: (その節の見出し 左部分木 右部分木)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; => q2.63: 二進木をリストに変換する
;; => q2.64: 順序ありリストを"釣り合っている"二進木に変換する
;; => q2.65: 二進木集合のunion-set, intersection-setを実装

