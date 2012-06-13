(load "./q2.84")
(load "./q2.78") ;; tagなしscheme-number

;; 設問の意味をよく理解してなかった。
;; project - 強制投射. 有理数1.2を整数1にしてしまうような。
;; この問題の真意は
;;   projectしたあとraiseして等しい(この等しさはq2.79のequ?が使える) = 可逆的ならレベルを下げられる
;; というもの。
;;
;; そしてproject + raiseを使って「落ちるとこまで落とす」、raise-toのようなdropを定義しろという問題であった。

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

  (put 'project '(integer)
       (lambda (x) (error "Cannot project integer." x)))

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

  (put 'project '(rational)
       (lambda (x) (if (= (denom x) 1)
                     (make-integer (numer x))
                     (make-integer (round (/ (numer x) (denom x)))))))

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

  ;; 可能な限り標準手続きを活用。これでどや
  (put 'project '(scheme-number)
       (lambda (x) (cond ((integer? x)
                          (make-rational x 1))
                         (else
                           (let ((r (inexact->exact x)))
                             (make-rational (numerator r)
                                            (denominator r)))))))

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

  (put 'project '(complex)
       (lambda (z) (make-scheme-number (real-part z))))

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

(define (project x) (apply-generic 'project x))

;; ----------------------------------------------

;; (define (install-project-package)
;;   (define (complex->scheme-number x)
;;     (attach-tag 'scheme-number (real-part x)))
;;   (define (scheme-number->rational x)
;;     (if (inexact? (contents x)) 
;;       (attach-tag 'scheme-number x)
;;       (make-rational x 1))
;;     (define (rational->integer x)
;;       (attach-tag 'integer ())))





;; ------------------

;; gosh> (raise (project r1))
;; (rational 2 . 1)
;; gosh> (raise (project r2))
;; (rational 2 . 1)
;; gosh> r1
;; (rational 2 . 3)
;; gosh> r2
;; (rational 2 . 1)


;; equ?を使う
;;   このシステムを客観的に見るとdropしないほうがいい。
;;   明示的にcastするとかにしたほうがいい


