; sec2.2.2のcount-leavesをaccumulationで再定義

; 再掲 accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate
    (lambda (leaves total) (+ leaves total)) ; 適用operationはtotalへの足し合わせ
    0 ; 初期葉の数は0
    (map (lambda (sub-tree) ; accumulateにsequenceとして渡すのは、tに対してmapでcount-leavesしたもの
           (if (pair? sub-tree)
                      (count-leaves sub-tree)
                      1)) ; pairじゃなければ葉を1個カウントアップ
           t)))

; ちょっと裏技っぽい方法もある。さっきのflatten(仮)を使ってやる。
(define (count-leaves2 t)
  (accumulate + 0 (map (lambda (x) 1) (enumulate-tree t))))


; うまくいかない。
(define (count-leaves3 t)
  (accumulate + 0 (map (lambda (subtree)
                         (cond ((null? subtree) 0)
                               ((pair? subtree) (count-leaves3 subtree))
                               (else (accumulate + 0 subtree)))))))



