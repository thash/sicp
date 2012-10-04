;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.2. 無限ストリーム(Infinite Streams) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "./stream")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; これまで定義したstreamの仕組みで、既に無限に続く対を扱えるようになっている。
; gosh> integers
; (1 . #<closure (memo-proc memo-proc)>)
; gosh> (stream-ref integers 100)
; 101

;; たとえば7で割り切れない整数のstream, なども定義できる
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

; gosh> no-sevens
; (1 . #<closure (memo-proc memo-proc)>)

; こいつはstream-refで参照できる
; gosh> (stream-ref no-sevens 100)
; 117

;; 無限ふぃぼなっち数列(Fibonacci numbers)を作ってみよう
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

; gosh> (stream-ref fibs 10)
; 55

;; no-sevensを一般化し、
;; かの有名なエラストテネスの篩 (sieve of Eratosthenes) で無限の素数ストリームを作ろう.
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
; gosh> (stream-ref primes 22)
; 83

;; 豆(脚注60): エラストテネスの篩は古典的な素数の探し方であり、1970年代からは1.2.6節で見たような統計的技法で置き換えられている。


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ストリームの暗黙の定義 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ここまでのように明示的なstreamの"生成"をする他、暗黙のdelay効果を使う手もある。
;; 次のように、再帰っぽくstreamを定義してしまう。
(define ones (cons-stream 1 ones))
;; (1 1 1 1 ...) と1だけのstream.

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; integersをさっきとは違う方法で定義する。
;;   * integersの第2要素は1+[integersの第1要素つまり1] = 2
;;   * integersの第3要素は1+[integersの第2要素つまり2] = 3
(define integers (cons-stream 1 (add-streams ones integers)))

;; Fibonacciも同じように定義できるよ。
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;; https://img.skitch.com/20120827-ebs22bx6naindkja7juc6juyiw.jpg

;; scale-streamを用意する。streamの各要素に与えられた定数を掛ける。
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; 例 -- 2のべき乗stream.
(define double (cons-stream 1 (scale-stream double 2)))

;; Yet Another素数streamの定義
(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))

;; prime? の定義はこんな感じ。nをsqrt(n)以下の素数で割れるかどうか判定する。
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;; primesの定義にprime?を使い、prime?手続きにprimesが登場するという再帰的定義。

;; "この手続きが動く理由は、次に素数性をチェックしようとする数に対して、いつでもテストするのに十分なだけのprimesストリームが生成されているからである。
;; つまり、素数性をテストしようとするあらゆるnに対して、
;;   * nは素数でない(この場合, nを割り切る素数は既に生成されている)か、
;;   * nは素数である(この場合、sqrt(n)より大きい素数--nよりは小さい--が生成されている)か、
;; である。"


