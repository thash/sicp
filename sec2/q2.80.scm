;; 引数がゼロかどうかテストする汎用述語=zero?を定義し、汎用算術演算パッケージに設定せよ。
(load "./sec3.3.3") ;; get, put
(load "./sec2.4.2") ;; attach-tag, contents, etc
(load "./sec2.4.3") ;; install-(rectangular/polar)-package, apply-generic
(load "./sec2.5.1") ;; install-scheme-number/rational-package, complex again
(load "./q2.78")    ;; new attach-tag, contents, type-tag

(put '=zero? '(scheme-number)
     (lambda (x) (eq? (contents x) 0)))

;; numerはrational packageの内部手続きとしてしか定義してなかったため外部から定義するのが難しい...全部書く。
(define (install-rational-package)
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
  (define (tag x) (attach-tag 'rational x))
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

  ;; ここに追加
  (put '=zero? '(rational)
       (lambda (x) (eq? (numer x) 0)))

  'done)

;; complexも同じく, imag-partを使いたかったので内部に定義
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

  ;; ここに追加
  (put '=zero? '(complex)
       (lambda (x) (eq? (imag-part x) 0)))

  'done)

;; 汎用手続き化
(define (=zero? x) (apply-generic '=zero? x))
