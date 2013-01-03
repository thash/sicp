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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 正確な作用順じゃないと思うけど展開してみる.

(define x (list 1 2 3))

(count-leaves x)

(accumulate (lambda (leaves total) (+ leaves total))
            0
            (map (lambda (sub-tree) (if (pair? sub-tree) (count-leaves sub-tree) 1))
                 x))

(accumulate (lambda (leaves total) (+ leaves total))
            0
            (map (lambda (sub-tree) (if (pair? sub-tree) (count-leaves sub-tree) 1))
                 '(1 2 3)))

(accumulate (lambda (leaves total) (+ leaves total))
            0
            '(1 1 1)) ; mapにより sub-tree = 1 として3個実行され, すべてpair? => #fなので1

;; accumulateに渡すべきsequenceができたので, 次はaccumulateの中身.
;; '(1 1 1) != null なのでop が実行される.
(op (car '(1 1 1))
    (accumulate op 0 '(1 1)))

(op (car '(1 1 1))
    (op (car '(1 1))
        (accumulate op 0 '(1))))

(op (car '(1 1 1))
    (op (car '(1 1))
        (op (car '(1))
            (accumulate op 0 '()))))

(op (car '(1 1 1))
    (op (car '(1 1))
        (op (car '(1))
            0))) ; null?に該当しようやく再帰が止まる.

;; carを開く
(op 1 (op 1 (op 1 0)))

;; 今回はop = leaves,totalという引数付きで定義されたlambdaなので
(lambda 1 (lambda 1 (lambda 1 0)))
; で, lambdaの本体 => (+ arg1 arg2) ... lambda作用時の表記がわからん
(+ 1 (+ 1 (+ 1 0)))
3
