(add-load-path ".")
(load "q2.7")

; 本文
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (lower-bound i) (upper-bound i)) 2))

; が、パーセント許容誤差で定義したいそうだ。20%は0.2ではなく20と表すことにする。

; 中央値とパーセント相対許容誤差をとり、区間を返すmake-center-percentを定義する
(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100)))
                 (+ c (* c (/ p 100)))))

; ちょっと簡単にこれでもいけるはず

(define (make-center-percent c p)
  (make-interval (* c (- 1.0 (/ p 100)))
                 (* c (+ 1.0 (/ p 100)))))
