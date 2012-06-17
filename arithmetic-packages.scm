(load "./sec3.3.3") ;; put/get

;; tags with scheme-number - q2.78.scm {{{
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents))) ;; }}}

;; >>> sec2.4.3.scm, q2.83.scm, q2.85.scm --------------------------------------------
; 整数(integer) --------------------- {{{
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
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
       (lambda (n) (make-rational n 1)))

  'installed.) ;; }}}

; 有理数(rational) --------------------- {{{
(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))
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
  (put 'equ? '(rational rational)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(rational)
       (lambda (x) (eq? (numer x) 0)))

  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (* 1.0 (/ (numer x) (denom x))))))

  'installed.) ;; }}}

; 実数(scheme-number) --------------------- {{{
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

   (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
   (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
   (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
   (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (eq? (contents x) 0)))

  (put 'raise '(scheme-number)
       (lambda (n) (make-complex-from-real-imag n 0)))
  'installed.) ;; }}}

; 複素数(complex - rectangular+polar) ---------------------{{{
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) 
    (sqrt (+ (square (real-part z))
              (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x)) 
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) 
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x)) 
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

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
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; from q2.77 - additional-complex
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ? '(complex complex)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(complex)
       (lambda (x) (eq? (imag-part x) 0)))

  (put 'project '(complex)
       (lambda (z) (if (= (imag-part z) 0)
                     (make-scheme-number (real-part z))
                     (tag z))))

  'installed.) ;; }}}

;; 汎用手続き化
(define (make-integer n) ((get 'make 'integer) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
;; <<< sec2.4.3.scm, q2.83.scm, q2.85.scm --------------------------------------------

(install-integer-package)
(install-rational-package)
(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; >>> tower, raise - q2.84.scm -------------------------------------------- {{{
(define tower '(integer rational scheme-number complex))

(define (higher type1 type2)
  (let ((type1-pos (memq type1 tower))
        (type2-pos (memq type2 tower)))
    (if (or (eq? #f type1-pos) (eq? #f type2-pos))
      (error "Error: invalid type(s)." type1 type2)
      (cond ((= (length type1-pos) (length type2-pos)) type1)
            ((< (length type1-pos) (length type2-pos)) type1)
            ((> (length type1-pos) (length type2-pos)) type2)))))

(define (raise-to n target-type)
  (if (eq? (type-tag n) target-type)
    n
    (raise-to (raise n) target-type)))
;; <<< q2.84.scm -------------------------------------------- }}}

;; q2.85.scm -- まだ入れてない。q2.85で通らないテストがあるので....
;; drop組み込み版、"単純化"機能付きのapply-generic.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        ;(drop (apply proc (map contents args))) -- これうまくいかない...
        (if (not (= (length args) 2))
          (error "[args] Error: No method for these types" op type-tags)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (eq? type1 type2)
              (error "[Same type] Error: No method for these types" op type-tags)
              (let ((target-type (higher type1 type2)))
                (apply-generic op (raise-to a1 target-type) (raise-to a2 target-type))))))))))


