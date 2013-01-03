(add-load-path ".")
(load "my_defs")

;; Fermatの小定理: nを素数, aをnより小さい正の任意の整数とすると,
;;                 aのn乗はnを法としてaと合同である. つまり a^n ≡ a (mod n)

;; 与えられた数 n をテストするには, a < nなaをランダムに取り a^n % n を計算.
;; a^n % n == aであればnは素数"かも"しれず, まだテストの余地がある. 繰り返せばより確信が持てる.
;; a^n % n != aであればnは"確実に素数ではない".


;; a^n % nを計算する手続きexpmod, テキストの言葉で言うと
;; "ある数のべき乗の別の数を法とした剰余を計算する" 手続きを用意する.
(define (expmod base exp m)
  (cond ((= exp 0) 1) ;; 再帰の終了条件.
        ;; 肩の数が偶数なら二乗を計算.
        ;; これは単なるショートカットで, 愚直に1個ずつかけても結果は同じ
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (square x) (* x x))

;;;;;;;;;;;;;;;;

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(expmod 2 2 2)
