; 本文
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (lower-bound i) (upper-bound i)) 2))

; が、パーセント許容誤差で定義したいそうだ。

; 中央値とパーセント相対許容誤差をとり、区間を返すmake-center-percentを定義する
(define (make-center-percent c p)
 ; TODO 
  )


