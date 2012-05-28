(load "./q2.84")

;; q2.84.scm にdropを追加
; 整数 ---------------------
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x)) ; {{{

  ;; integerの四則演算を定義
  (define (add-int x y) (+ x y))
  (define (sub-int x y) (- x y))
  (define (mul-int x y) (* x y))
  (define (div-int x y) (/ x y))
  (put 'add '(integer integer)
       (lambda (x y) (tag (add-int x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (sub-int x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (mul-int x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (div-int x y))))
  (put 'make 'integer
       (lambda (n) (tag n)))
  (put 'raise '(integer)
       (lambda (n) (make-rational n 1))) ; }}}

  (put 'drop '(integer)
       (lambda (x) (error "Cannot drop integer." x)))

  'installed.)

; 有理数 ---------------------
(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x)) ; {{{
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

   (define (add-rat x y)
     (make-rat (+ (* (numer x) (denom y))
                  (* (numer y) (denom x)))
               (* (denom x) (denom y))))
   (define (sub-rat x y)
     (make-rat (- (* (numer x) (denom y))
                  (* (numer y) (denom x)))
               (* (denom x) (denom y))))
   (define (mul-rat x y)
     (make-rat  (* (numer x) (numer y))
                (* (denom x) (denom y))))
   (define (div-rat x y)
     (make-rat  (* (numer x) (denom y))
                (* (denom x) (numer y))))
 
   (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
   (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
   (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
   (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
   (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x))))) ; }}}

  (put 'drop '(rational)
       (lambda (x) (if (= (denom x) 1)
                     (make-integer (numer x))
                     (tag x))))

  'installed.)

; 実数(scheme-number) ---------------------
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

   (put 'add '(scheme-number scheme-number) ; {{{
        (lambda (x y) (tag (+ x y))))
   (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
   (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
   (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise '(scheme-number)
       (lambda (n) (make-complex-from-real-imag n 0))) ; }}}

  (put 'drop '(scheme-number)
       (lambda (x) (if (inexact? x)
                     (tag x)
                     (make-rational x 1))))

  'installed.)

; 複素数(complex) ---------------------
(define (install-complex-package)
  ;; 直行座標と極座標パッケージから取り入れた手続き {{{
  ;;  -- rectangularとpolarの手続きが既に入って無いと行けない。
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; システムの他の部分へのインターフェイス
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a)))) ; }}}

  (put 'drop '(complex)
       (lambda (z) (if (= (imag-part z) 0)
                     (make-scheme-number (real-part z))
                     (tag z))))

  'installed.)


(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (drop x) (apply-generic 'drop x))
