;; > 驚いたことに問題4.20でのLouisの直感は正しい.
;; Louisの扱いワロタ
;;
;; lambdaだけでletやdefineを定義しようぜというお話. おもしろいところ

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 10)

;; 定義したその場で適用しようとするlambdaは((lambda (..) (...)) args) となってる.


;; (a). 式を評価して実際に10の階乗を計算できることを確認せよ
; gosh> ((lambda (n)
;    ((lambda (fact)
;       (fact fact n))
;     (lambda (ft k)
;       (if (= k 1)
;         1
;         (* k (ft ft (- k 1)))))))
;  10)
; 3628800

;; また, Fibonacci数を計算する類似の式を考えよ.
;
;; ふつーのFibonacci An+1 = An + An-1
;    (define (fib n)
;      (if (< n 3)
;        1
;        (+ (fib (- n 1)) (fib (- n 2)))))

;; 自分自身の名前"fib"が使えないあたりを工夫する.
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (< k 3)
        1
        (+ (fb fb (- k 1)) (fb fb (- k 2)))))))
 7)


;; (b). 相互に再帰的な内部定義を持つ次の式を考える.
;; 何度か観たけど要はxを初期値として1ずつ引いて落とし込み, 0になったポイントで偶奇を判定している
(define (f x)
  (define (even? n)
    (if (= n 0)
      #t
      (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
      #f
      (even? (- n 1))))
  (even? x))

;; 内部定義もletrecも使わないfの定義法(部分ヒントあり)を完成せよ.
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))


