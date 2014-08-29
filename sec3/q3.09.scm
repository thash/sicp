
;; 再帰版
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; 反復版
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

;; それぞれの版について、(factorial 6) を評価するときの環境構造を示せ。
;; 注釈14: 環境モデルからは、fact-iter手続きが「末尾再帰」になっていることを判別できない。5.4節までのおたのしみ。
