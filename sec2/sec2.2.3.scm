;;; 2.2.3. 公認インターフェイスとしての並び(sequence)
;;;       (conventional interface)
(add-load-path ".")
(load "my_defs")
(load "q1.19")

(define (sum-odd-squares tree)
  (cond [(null? tree) 0]
        [(not (pair? tree))
         (if (odd? tree) (square tree) 0)]
        [else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree)))]))
(trace sum-odd-squares)

(define x (list (list 1 2 (list 3 4 (list 5 6) 7 ))))
(define y (list 1 2 3))

; fib書かないと動かないが。 q1.19内に定義がある。
(define (even-fibs n)
  (define (next k)
    (if (> k n)
      ()
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))


; ↑二つの例は "map+filter+accumulate" という共通構造を持つ。
; 信号処理でやっていることらしい。

; filter -- Rubyで言うselectを実装する。
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; [2nd]20130819 手続き名がfilterの時のみ失敗する...
;;      (even?, mapなどの名前なら既存の手続き上書きしても大丈夫だった).
;;      のでmy-filterかにすると良いのかな
; gosh> (filter odd? (list 1 2 3 4 5))
; (1 3 5)

; accumulate -- Rubyで言うinjectを実装する。
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; gosh> (accumulate + 0 (list 1 2 3 4 5 6 7 8 9))
; 45

; 処理すべき要素の並びを数え上げる
(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

; 木の葉を数え上げる. Rubyで言うflatten.
(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

; 信号処理方式でsum-odd-squaresを書き直す。
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

; enumerate-treeで処理すべき葉をリストアップし、odd?でfilterする。
; accumulate, filter, sum-odd-squaresすべてをtraceしてみると
;
; gosh>  (sum-odd-squares x)
; CALL sum-odd-squares ((1 2 (3 4 (5 6) 7)))
;   CALL filter #[proc] (1 2 3 4 5 6 7)
;     CALL filter #[proc] (2 3 4 5 6 7)
;       CALL filter #[proc] (3 4 5 6 7)
;         CALL filter #[proc] (4 5 6 7)
;           CALL filter #[proc] (5 6 7)
;           RETN filter (5 7)
;         RETN filter (5 7)
;       RETN filter (3 5 7)
;     RETN filter (3 5 7)
;   RETN filter (1 3 5 7)
;   CALL accumulate #[proc] 0 (1 9 25 49)
;     CALL accumulate #[proc] 0 (9 25 49)
;       CALL accumulate #[proc] 0 (25 49)
;         CALL accumulate #[proc] 0 (49)
;           CALL accumulate #[proc] 0 ()
;           RETN accumulate 0
;         RETN accumulate 49
;       RETN accumulate 74
;     RETN accumulate 83
;   RETN accumulate 84
; RETN sum-odd-squares 84
; 84
;
; こうなる。

(define (even-fibs n)
  (accumulate cons
              ()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

; 最初のn+1個のFibonacci数の二乗リスト
(define (list-fib-squares n)
  (accumulate cons
              ()
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

; 並び(sequence)中の奇数の二乗を掛け合わせる(何のためにだよ
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

; gosh> (product-of-squares-of-odd-elements (list 1 2 3 4 5))
; 225

; 最高収入のプログラマを見つける (例なのでsalaryとかprogrammer?を定義しないと動かない)
(define (salary-of-highest-paied-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

