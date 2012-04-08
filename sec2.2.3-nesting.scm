; Nested Mappings
; ある正の整数nに対し 1 <= j < i <= n である異なる整数jとiの順序対で、i+jが素数になるものを選択する

; enumerate-interval -- sec2.2.3より
(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

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
;                    (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 n)))

; mappingとaccumulatingの組み合わせを一般化しよう
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

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
(define (permutations s)
  (if (null? s)
    (list ())
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

; ↑のremoveはこうやって定義する
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

