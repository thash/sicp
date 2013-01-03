;; 1.2.6 例: 素数性のテスト
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; > ある数が素数であるかどうかをテストする方法の一つは
;; その除数を見つけることである.
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

;; 事実F:"nが素数でなければ, ルートn以下の除数を持つ" に基づいたfind-divisor.
;; Fの証明は興味深いが, また別のお話.
;; ルートnに達するまでtest-divisorを1ずつ増やし最初(=最小)の割り切れる数を探す.
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor) ;#?= for test-divisor
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; ある数を割り切れる数(除数)が1とその数自身のみであるとき
;; (= 最小除数が自分自身であるとき), その数は素数である.
(define (prime? n)
  (= n (smallest-divisor n)))

;; (print (prime? 4)) ;;=> #f (false)
;; (print (prime? 3)) ;;=> #t (true)


;(print (prime? 209))
