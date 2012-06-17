; 2.4.1. 複素数
; 直行座標形式, 極座標形式の2通りで表現できる。
; 実部x, 虚部yの(x,y)を
; 絶対値r, 偏角Aの(r,A)に結びつける方法は以下の通り。
;
; x = r cos A, y = r sin A
; r = sqrt(x^2+y^2), A = arctan(y,x)
;
; 二つの表現で扱えるようなデータ構造にしたい。そのためにタグ付きデータを導入する。


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (display "error")))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (display "error")))

; 直行座標系 - rectangular
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

; 極座標系 - polar
(define (polar? z)
  (eq? (type-tag z) 'polar))

; 両者の共存したデータ体系
;   rectangular version.
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;   polar version. それぞれ得意な計算が違う。
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


; データの取得
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (display "error"))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (display "error"))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (display "error"))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (display "error"))))


; 汎用的な計算。real-partなどデータ取得手続きで差異を吸収しているため形式によって対応を変える必要が無い。
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-real-imag (* (magnitude z1) (magnitude z2))
                       (+ (angle z1)     (angle z2))))

(define (div-complex z1 z2)
  (make-from-real-imag (/ (magnitude z1) (magnitude z2))
                       (- (angle z1)     (angle z2))))

; 複素数をrectangularで構成するか、polarで構成するか
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

