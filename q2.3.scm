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
    (y-point (start-segment seg))))
  )))

;  a +
;    |
;    |
;  b +------+ c
; ac^2 = ab^2 + bc^2 (90...right angle)

(define (make-rectangle a b c) ; a,b,c are points.
  (cons (make-segment a b) (make-segment b c)))
  ; [better?] if not 90, exit...
  ; (define length-ab (seg-length (make-segment a b)))
  ; (define length-bc (seg-length (make-segment b c)))
  ; (define length-ca (seg-length (make-segment c a)))

(define (rect-width rect) (seg-length (car rect)))
(define (rect-height rect) (seg-length (cdr rect)))

; usage: (define r1 (make-rectangle (make-point 4 0) (make-point 0 0) (make-point 5 0)))

(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

