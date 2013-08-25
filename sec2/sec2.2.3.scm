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

; Nested Mappings
; ある正の整数nに対し 1 <= j < i <= n である異なる整数jとiの順序対で、i+jが素数になるものを選択する

; enumerate-interval -- sec2.2.3より
(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

; Tips: consでlistを作る時の注意。最終項とnilをconsするべき
; gosh> (cons 1 (cons 2 (cons 3 4)))
; (1 2 3 . 4)
; gosh> (cons 1 (cons 2 (cons 3 (cons 4 ()))))
; (1 2 3 4)

; accumulate -- sec2.2.3より
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; 実装 -- "For each interger i<=n, enumerate the intergers j<i, and for each such i and j generate the pair (i,j). In terms of sequence operations, we map along the sequence (enumerate-interval 1 n). For each i in this sequence, we map along the sequence (enumerate-interval 1 (- i 1)). For each j in this latter sequence, we generate the pair (list i j). This gives us a sequence of pairs for each i. Combining all the sequences for all the i (by accumulating with append) produces the required sequence of pairs"
;
; (accumulate append
;             nil
;             (map (lambda (i)
;                    (map (lambda (j) (list i j))  ; <= ここのlistで, iやjといったlistをlistしてる
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 n)))

; mappingとaccumulatingの組み合わせを一般化しよう
; listのlistから1つのlistを作るflatmap.
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
;; procの使い所が今ひとつ...
; gosh> (flatmap + (list (list 3 4) (list 1 2)))
; (3 4 1 2)

; 対の和が素数になるものを発見するfilter
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; 最後に、対の二つの要素とその和でpairを作る。pairとは言うが実装的にはlist (cadrしてるし)
(define (make-pair-sum pair)
  #?=(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; 以上より結論。
(define (prime-sum-pairs n)
  (map (make-pair-sum
         (filter prime-sum?
                 (flatmap
                   (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))))


; この手続きの別の応用。集合Sのすべての順列を生成する。
; permutation of (1 2 3) is ((1 2 3) (1 3 2)...)
; require "flatmap"
(define (permutations s)
  (if (null? s)
    (list ())
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s)))) ; 除いたものとconsする
             s)))

; ↑のremoveはこうやって定義する
; require "filter"
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

