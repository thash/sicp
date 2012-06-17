(load "./q2.84")
;; (load "./q2.78") ;; tagなしscheme-number .. 79に含まれるのでコメントアウト
(load "./q2.79") ;; equ?.

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
       (lambda (x) (make-integer x)))

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
  ;; arithmetic-packages.scm とズレてる
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (equ? (contents x) 0)))
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

;; 2.79で定義したequ?を使う.
;; 【ハマった】
;; integerの場合はprojectしても自分自身になるように設計したため、
;; 判定もそれに合わせてintegerならdroppable?を常にfalseにする。
(define (droppable? x) 
  (if (eq? (type-tag x) 'integer) #f
  (equ? x (raise (project x)))))

(define (drop x)
  (if (droppable? x)
    (drop (project x))
    x))

;; そして最後にapply-genericを書き換える。これは、そのままだと
;; (add (make-rational 4 3) (make-rational 5 3))
;; の結果が(rational 3 . 1)になるから(integer . 3)まで落とすようにしましょうという話
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define maybe-drop
      ;; apply-genericで実行する手続きが以下に含まれる場合、maybe-dropは引数をそのまま返す無名関数になる。
      (if (memq op '(equ? =zero? raise project))
        (lambda (x) x)
        drop))
   ;; (trace maybe-drop)
    (let ((proc (get op type-tags)))
      (if proc
        ;(and (and (newline) (display op))
        ;     (maybe-drop (apply proc (map contents args))))
        ;;;; (maybe-drop (apply proc (map contents args)))
        (apply proc (map contents args))
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

;; 結果返すところにdrop かけるだけだと無限ループになるらしくうまくいかない。dropの実装を見直す...

;; from: http://oss.timedia.co.jp/show/SICP/ex-2.85
;; これに置き換えてみても同じ... 問題はdrop以外、別の所にあるらしい。
;; (define (drop x)
;;   (cond ((boolean? x) x)
;;         ((eq? (type-tag x) 'integer) x)
;;         (else (let ((dropped (project x)))
;;                 (if (equ? x (raise dropped))
;;                     (drop dropped)
;;                     x)))))
;;
;;  同じく↑のページより、drop適用時にいくつかapply-genericのdrop適用から外すべきものがあったらしい。
;;  maybe-dropがとして内部手続きとして定義。 これを使うとうまくいった！
;; (define maybe-drop
;;   (if (memq op '(equ? =zero? raise project))
;;     (lambda (x) x)
;;     drop))
;;  理由は、apply-genericはいろんな場所に使われてしまっていたので、
;;  drop内で使うprojectやraiseでさらにapply-genericが呼ばれ、無限手続き呼び出しが発生していた。

;; [余談]
;; このシステムはこーゆー問題だから作ったけど、客観的に見るとdropしないほうがいい。
;; 明示的にcastする、とかにしたほうがいい

;;      CALL equ? 10/3 10/3
;;      RETN equ? #f

