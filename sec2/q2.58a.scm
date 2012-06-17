(add-load-path ".")
(load "q2.57")

; ここで微分手続きは抽象的に定義されているので(Schemeのsyntaxから外れて)数学的表記にしてもOKに出来るはずである、と。つまり
; (+ x y) => (x + y) とできるようにする。

; (a). 簡単のため"2つの項を取り", "かっこで囲まれている"と仮定する。

; どうもlistでやると+が特殊解釈されてしまう? ので、quote+()で作る。
; gosh> (list '2 + 3)
; (2 #<subr +> 3)
; gosh> (eq? '+ (cadr (list '2 + 3)))
; #f
; gosh> '(2 + 3)
; (2 + 3)
; gosh> (eq? '+ (cadr '(2 + 3)))
; #t
;
; ...と思ったが単に(list 2 '+ 3)とすればよかった。これで(2 + 3)になるわ

; sum, product, exponentiation系を再定義しよう。
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
; (addend e) 加数 -- 和式listの第1項
(define (addend s) (car s))
; (augend e) 被加数 -- 和式listの第三項 - 2項の和のみ考慮。
(define (augend s) (caddr s))

; makeするとき順番を変える。"実際に"足すときはSchemeに解釈させないといけないので(+ a1 a2)の順。
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

; 同様にproductとexponentiationを実装する。
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base x)     (car  x))
(define (exponent x) (caddr x))
(define (power x n) (if (= n 1) x (* x (power x (- n 1)))))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (power base exponent))
        (else (list base '** exponent))))


; 構成手続きが変わったので、ここでq2.56と同じderivを定義しなおし。
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        (else)))


