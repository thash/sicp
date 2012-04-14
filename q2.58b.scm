(add-load-path ".")
(load "q2.58a")

; (b). より人間らしい形に近づける。不用なカッコは除き、複数の項を演算子で繋げるようにする。つまり
;      (x + 3 * (x + y + 2)) という表記を許す。

; そういえばこれは計算の優先順序も知ってないといけない。まあ後で考えよう。
; リストの要素を調べていって、演算子、加算数、被加算数を区別する処理をまず考える。積とかも同じく。
; sec2.3.1で定義したmemqが使えそうである。用意しておこう。

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; http://d.hatena.ne.jp/awacio/20100803/1280843906
; このへん見て、式全体が和か積かをまず区別することが出来ることに気付く。なるほど。

; memqはヒットしたときはヒットした要素を含めたそれ以降のlistを返す実装であった。単に#t, #fを返すsum?に組み込むにはちょっと面倒だがこうする。

(define (sum? x)
  (if (eq? #f (memq '+ x)) #f #t))

; gosh> (sum? '(x * y * 4))
; #f
; gosh> (sum? '(x * y + 4))
; #t
; よしよし。これができれば...

; memqは使えないので、前半を取ってくるのが逆に難しいな。再帰で回す。
; carで取った加算数が1個だけだった場合どうしよう。listじゃなくて本体を返したい。
(define (addend x)
  (if (not (pair? x)) ()
    (if (eq? (car x) '+) ()
      (let ((ans (cons (car x) (addend (cdr x)))))
        (if (null? (cdr ans)) (car ans)
          ans)))))

; addendで再帰的に求めた答えをansに束縛しておいて、cdrがnull(つまり要素数1のlist)である場合はansをcarした中身を返す。それ以外はansをそのまま返す。


; gosh>  (addend '(4 * x + 5 * y))
; (4 * x)
; (a + b + c) のときaだけ取れてくる形。b+cは後にする。

; augendの方は簡単。
(define (augend x) (cdr (memq '+ x)))

; gosh> (augend '(4 * x + 4 * y))
; (4 * y)

; これで多項式も扱えるようになった。
; gosh> (addend '(x * y * z + 3 * x + (x * x) * 4))
; (x * y * z)
; gosh> (augend '(x * y * z + 3 * x + (x * x) * 4))
; (3 * x + (x * x) * 4)

; ちなみにmake-sumは同じものを使う。


; 次、積について。
; 減算と除算はないものとする＞＜
; +があればproduct? => #fでいいとして、積であるかどうかを見るには、memqが返した要素をチェックして*が存在するかどうかを見てやらないといけない。
; つまりproduct?が#tとなるのは項中に+がなくて*があるとき。
(define (product? x)
  (if (eq? #f (memq '+ x))
    (if (eq? #f (memq '* x)) #f #t)
    #f))

; sumであることはありえないからcarで取ればそれが乗数
(define (multiplier x) (car x))

; 2項しかないケースをnull? cdddrで検知してその場合はlistでwrapしないそのものを返すようにする。
(define (multiplicand x)
  (if (null? (cdddr x))
    (caddr (memq '* x))
    (cdr (memq '* x))))

; gosh> (multiplier '(x * y * z * 3 * x * (x * x) * 4))
; x
; gosh> (multiplicand '(x * y * z * 3 * x * (x * x) * 4))
; (y * z * 3 * x * (x * x) * 4)

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

; gosh> (deriv '(x + (3 * ((x * x) + (x * (y + 2))))) 'x)
; (1 + (3 * ((x + x) + (y + 2))))
;
; できた！！
