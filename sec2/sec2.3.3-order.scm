; 順序づけられたリストとしての集合
; 順序づけることで、element-of-set?において全要素を走査する必要がなくなり、効率的になる。

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; 数の大きさを見て途中で中断できる。走査するために平均してn/2のステップ数が必要となるため、
; オーダーは変わらずO(n)であるが、2倍の改善が見込める。

; intersection-setはO(n^2) -> O(n)になる。なんとなれば、

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
    (let ((x1 (car set1)) (x2 (car se2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1) (cdr set2)))) ; 走査対象set2も縮めていくことができる
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2))) ; set2のcar要素、次の奴が取れるようにする
            ))))

; set1 and/or set2 の最初の要素を除去して、より小さな集合の問題に引き下げている。


