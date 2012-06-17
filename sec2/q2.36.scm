; accumulate-n は第三引数として「"すべてが同数の要素からなる並び"の並び」を取る他はaccumulateと同じ。
; accumulationとして指定した手続きを、並びの全ての第一要素、第二要素...と作用させ、結果の並びを返す。
; Rubyで言うzip?

; 例
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)
; (cons (+ 1 4 7 10) (cons (+ 2 5 8 11) (cons (+ 3 6 9 12) ())))
; => (22 26 30)

; 再びaccumulate定義
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs)) ; 一個下を見なければならない
    ()
    (cons (accumulate op initial (map car seqs))
          (accumulate-n op initial (map cdr seqs)))))

