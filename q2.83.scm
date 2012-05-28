(load "./sec2.5.2") ;; includes 2.4.2, 2.4.3, 2.5.1, 3.3.3, q2.77

;; 整数(integer) ⊆ 有理数(rational) ⊆ 実数(real) ⊆ 複素数(complex)

; 整数 ---------------------
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))

  ;; integerの四則演算を定義 {{{
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
       (lambda (x y) (tag (div-int x y)))) ; }}}

  (put 'make 'integer
       (lambda (n) (tag n)))
  (put 'raise '(integer)
       (lambda (n) (make-rational n 1)))

  'installed.)

; 有理数 ---------------------
(define (install-rational-package) ; 元 sec2.5.1.scm
  (define (tag x) (attach-tag 'rational x))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

   (define (add-rat x y) ; {{{
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
        (lambda (n d) (tag (make-rat n d)))) ; }}}

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))

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
        (lambda (x y) (tag (/ x y)))) ; }}}

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise '(scheme-number)
       (lambda (n) (make-complex-from-real-imag n 0)))

  'installed.)


(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (raise x) (apply-generic 'raise x))

