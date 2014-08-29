(add-load-path ".")
(load "q2.2")

(define (seg-length seg)
  (sqrt
    (+
      (square (-
                (x-point (end-segment seg))
                (x-point (start-segment seg))))
      (square (-
                (y-point (end-segment seg))
                (y-point (start-segment seg)))))))

;; これ1週目で作った関数だけど別にsegment引数にとる必要ないよね
(define (square x) (* x x))
(define (distance p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))
;; gosh> (define p1 (make-point 1 1))
;; gosh> (define p2 (make-point 2 2))
;; gosh> (distance p1 p2)
;; 1.4142135623730951

;; ;;; 2013/07/08 これはひどい. 長方形なら2点決めれば定義できる. 何が90度だ死ね
;; ;  a +
;; ;    |
;; ;    |
;; ;  b +------+ c
;; ; ac^2 = ab^2 + bc^2 (90...right angle)
;;
;; (define (make-rectangle a b c) ; a,b,c are points.
;;   (cons (make-segment a b) (make-segment b c)))
;;   ; [better?] if not 90, exit...
;;   ; (define length-ab (seg-length (make-segment a b)))
;;   ; (define length-bc (seg-length (make-segment b c)))
;;   ; (define length-ca (seg-length (make-segment c a)))

;; 2点決めれば長方形が定義できる.
;; ただの2点のconsで表現上はsegmentと同じなんだけどrectangleとして扱う.
;; 他の定義法としてはheight/widthを与えてconsするというのもあるな
(define (make-rectangle p1 p2)
  (cons p1 p2))
(define (diagonal-point-former rect) (car rect))
(define (diagonal-point-latter rect) (cdr rect))

;; (define (rect-width rect) (seg-length (car rect)))
;; (define (rect-height rect) (seg-length (cdr rect)))
;;    + p1(x1,y1)
;;    |
;;    |
;;    +------+ p2 (x2,y2)
(define (rect-width rect)
  (abs (- (x-point (diagonal-point-former rect))
          (x-point (diagonal-point-latter rect)))))
(define (rect-height rect)
  (abs (- (y-point (diagonal-point-former rect))
          (y-point (diagonal-point-latter rect)))))

; usage: (define r1 (make-rectangle (make-point 4 0) (make-point 0 0) (make-point 5 0)))

(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

