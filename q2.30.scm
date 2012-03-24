(add-load-path ".")
(load "sec2.2")

; すべての要素にfactorを掛ける
(define (scale-tree tree factor)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

; 木は部分木から出来ていると考え、mapを使って実装する方法もある。
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree-map sub-tree factor)
           (* sub-tree factor)))
       tree))

; 本題, Q2.30.
(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; sample tree data
(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

