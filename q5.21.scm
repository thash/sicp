;; 次の手続きのレジスタ計算機を実装せよ. リスト構造用のメモリ演算は, 計算機の基本演算として使用可能とする.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (a). 再帰的count-leaves:
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; 解き方の指針
;; treeをassignでがりがり上書きしつつvalを増やしていく.
;; treeは"現在着目しているtree"という理解で良いか

;; 写経 http://www.serendip.ws/archives/3397

(define count-leaves-machine-a
  (make-machine
    '(continue tree val val-tmp tmp)
    ;; Quasiquote
    `((null? ,null?) (pair? ,pair?) (not ,not) (car ,car) (cdr ,cdr) (+ ,+))
    '(start
       (assign continue (label count-leaves-done))
       (assign val (const 0))
       count-leaves-loop
       (test (op null?) (reg tree))
       (branch (label null))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ; tree is not pair
       (branch (label not-pair))
       (save continue) ; continueを退避
       (assign continue (label count-leaves-with-car)) ; (count-leaves (car tree)) 的な処理へ行かせる
       (save tree) ; tree を退避
       (assign tree (op car) (reg tree)) ; treeの方もcarを入れて準備しておく
       (goto (label count-leaves-loop))
       null
       (assign val (const 0))
       (goto (reg continue))
       not-pair
       (assign val (const 1))
       (goto (reg continue))
       count-leaves-with-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label count-leaves-with-cdr)) ; こっちはcdrの方
       (save val)
       (goto (label count-leaves-loop))
       count-leaves-with-cdr
       (assign val-tmp (reg val))
       (restore val)
       (restore continue)
       (assign val (op +) (reg val) (reg val-tmp))
       (goto (reg continue))
       count-leaves-done)))

;; 動作テスト
; (set-register-contents! count-leaves-machine-a 'tree '(1 (2 3 (4)) (5 6)))
; (start  count-leaves-machine-a )
; (get-register-contents count-leaves-machine-a 'val)
; ;; => 6


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (b). カウンタを陽に持つ再帰的count-leaves:
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

;; 写経 http://www.serendip.ws/archives/3400
;; > 2つある再帰のうち1つは末尾再帰であるため
;; > (a)と比べて再帰呼び出しのための入り口のラベルが1つ減っている


(define count-leaves-machine-b
  (make-machine
    '(continue tree n val tmp) ; val-tmpがなくなりnが増えた
    ;; Quasiquote
    `((null? ,null?) (pair? ,pair?) (not ,not) (car ,car) (cdr ,cdr) (+ ,+))
    '(start
       (assign continue (label count-leaves-done))
       (assign n (const 0))
       count-leaves-loop
       (test (op null?) (reg tree))
       (branch (label null))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp))
       (branch (label not-pair))
       (save continue)
       (assign continue (label count-iter-with-car))
       (save tree)
       (assign tree (op cdr) (reg tree))
       (goto (label count-leaves-loop))

       null
       (assign val (reg n))
       (goto (reg continue))

       not-pair
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))

       count-iter-with-car
       ;; count-iter-with-cdr...はない
       (restore tree)
       (restore continue)
       (assign tree (op car) (reg tree))
       (assign n (reg val))
       (goto (label count-leaves-loop))

       count-leaves-done)))

;; 動作テスト
; (set-register-contents! count-leaves-machine-b 'tree '((1 2) 3 (4) (5 6 (7 8 (9 10)))))
; (start  count-leaves-machine-b )
; (get-register-contents count-leaves-machine-b 'val)
; ;; => 10
