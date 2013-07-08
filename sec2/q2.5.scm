;; aとbの対を2^a*3^bである整数で表現するなら,
;; 非負の整数の対は数と算術演算だけを使って表現できることを示せ.
;; この定義の元ではcons, car, cdrをどう表現することになるか.

;; 2と3は互いに素だからこういう表現ができるのか.
(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

;; consで"数をその形のまま"くっつけて表現するのもひとつの方法だが,
;; この問題のように"一意"に対応する数にひもづけることも可能.
;; gosh> (my-cons 2 4)
;; 324

(define (divcounter divnum z)
  (define (iter n count)
    (if (= 0 (remainder n divnum))
      (iter (/ n divnum) (+ count 1))
      count))
  (iter z 0))

(define (my-car z)
  (divcounter 2 z))
(define (my-cdr z)
  (divcounter 3 z))

;; gosh> (my-cons 2 4) ;;=> 324
;; gosh> (my-car 324)  ;;=> 2
;; gosh> (my-cdr 324)  ;;=> 4

