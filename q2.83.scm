(load "./sec2.5.2") ;; includes 2.4.2, 2.4.3, 2.5.1, 3.3.3, q2.77

;; 整数(integer) ⊆ 有理数(rational) ⊆ 実数(real) ⊆ 複素数(complex)

; install-*-package の中に以下の3つを定義する
;   - tag付け
;   - make-*
;   - raise

; 整数 ---------------------
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))

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

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))

  'installed.)

; 実数(scheme-number) ---------------------
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

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

