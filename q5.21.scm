;; 次の手続きのレジスタ計算機を実装せよ. リスト構造用のメモリ演算は, 計算機の基本演算として使用可能とする.

;; (a). 再帰的count-leaves:
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; (b). カウンタを陽に持つ再帰的count-leaves:
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

;; car-proc, ... find
;; cdr-proc, ...
;; treeをassignでがりがり上書きしつつvalを増やしていく.
;; treeは"現在着目しているtree"という理解で良いか

'(1 (2 3 (4)) (5 6)) ; などとしてcount. => 6, みたいな

