(add-load-path ".")
(load "q2.30")

; 2.30を抽象化. 任意の手続きをtreeの各要素に適用するtree-mapを作る。参考 - sec2.2.scm

(define (tree-map proc tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree-with-map tree)
  (tree-map square tree))
