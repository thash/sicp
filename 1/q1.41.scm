(add-load-path ".")
(load "my_defs")

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(trace inc)
(trace double)

(print
 (((double (double double)) inc) 5)
)

; (double double) で4回手続き
; (double (double double)) で8回手続き。2**3で8、でもある。
; となるので5にincを8回やれば答えは13
;  => 実行してみると21 orz
; 実際に展開してみよう。


(double (double double))
(double (lambda (x) (double (double x))))
;D = (lambda (x) (double (double x))) と仮置き
(double D)
(lambda (x) (D (D x)))
(lambda (x) ((lambda (x) (double (double x))) ((lambda (x) (double (double x))) x))) ;...という手続きが出来る。
;これをincに作用させる。

((lambda (x) ((lambda (x) (double (double x))) ((lambda (x) (double (double x))) x))) inc)
(lambda (inc) ((lambda (x) (double (double x))) ((lambda (x) (double (double x))) inc)))
((double (inc (inc))) ((double (inc (inc))) inc))
((((inc (inc)) ((inc (inc))))) (((inc (inc)) ((inc (inc))))) inc))

; わけわからん。lambdaで書くからダメなのか。
; http://www.serendip.ws/archives/476 のメモを見ると(double double)をこれで"手続き"とみなして展開してる。

((double (double double)) inc)
; (double double)をdoubleしたものをincにかける。
((double double) ((double double) inc))
; ((double double) inc)は何をするのか？ incをdoubleしたものをdoubleする
((double double) ((double (double inc))))
; incを(+ 1 x)と表すことで展開しやすくなる。 結局何が足されるのか、が見えないと。
((double double) ((double (double (+ 1 x)))))
((double double) ((double (+ 2 x))))
((double double) ((+ 4 x)))
((double double) (+ 4 x))
((double (double (+ 4 x)))
((double (+ 8 x))
((+ 16 x) ; x=5
(+ 16 5)
; => 21
