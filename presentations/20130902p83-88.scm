;; =============================================
;; p.83 2.3 記号データ
;; =============================================

;; 記号をデータとして扱うために新しい概念 "quote" を導入する.
(define a 1)
(define b 2)

(list a b) ;; => (1 2)
(list 'a 'b) ;; => (a b)

;; quoteを使って直接リストを作ることも出来る.
(car '(a b c)) ;; => a
(cdr '(a b c)) ;; => (b c)

;; '()で空リストを表す
'()

;; eq? を使ってmemqを実装する(Gaucheに存在するのでmy-menqとする)
(define (my-memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))

(my-memq 'apple '(pear banana prune)) ;; => #f
(my-memq 'apple '(x (apple sauce) y apple pear)) ;; => (apple pear)


;; q2.53
;; ---------------------------------------------

(list 'a 'b 'c)
;; => (a b c)
(list (list 'george))
;; => ((george))
(cdr '((x1 x2) (y1 y2)))
;; => ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;; => (y1 y2)
(pair? (car '(a short list)))
;; => #f
(car '(a short list))
;; => a
(memq 'red '((red shores) (blue socks)))
;; => #f
(memq 'red '(red shoes blue socks))
;; => (red shoes blue socks)


;; q2.54
;; ---------------------------------------------

;; (Gaucheには既にequal?が存在するのでmy-equal?とする)

;; 処理の本体は等価判定の再帰.

    (and (eq? (car a) (car b))
         (my-equal? (cdr a) (cdr b)))

;; cdrで食って行くと最後はnullになるのでそこを扱えるようにしておく.

           (cdr '(a b))        ;; => (b)
    (null? (cdr '(a b)))       ;; => #f
           (cdr (cdr '(a b)))  ;; => ()
    (null? (cdr (cdr '(a b)))) ;; => #t

;; * aとbが同時にnullになれば#t
;; * aとbのどちらかが先にnullになれば, そもそも要素数が違うということなので#f

        ;((or (and (null? a) (not (null? b)))
             ;(and (null? b) (not (null? a)))) #f)
(define (my-equal? a b)
  (cond ((and (symbol? a) (symbol? b) (eq? a b)) #t)
        ((and (null? a) (null? b)) #t)
        (else (and (pair? a) (pair? b)
                   (my-equal? (car a) (car b))
                   (my-equal? (cdr a) (cdr b))))))

;; 数値の時は = を使う (注釈)

    (my-equal? '(this is a list) '(this is a list)) ; => #t
    (my-equal? '(this is a list) '(this (is a) list)) ; => #f
    (my-equal? '(this is a list) '(this is not a list)) ; => #f


;; q2.55
;; ---------------------------------------------

    (car ''abracadabra)
    ;; => quote

;; 結果が quote となるのはなぜか.

;; http://www.schemers.org/Documents/Standards/R5RS/HTML/ から引用
;;
;;     > 4.1.2  Literal expressions
;;     >   syntax:  (quote <datum>)
;;     >   syntax:  '<datum>
;;     > (quote <datum>) may be abbreviated as '<datum>.

;; 'はquote手続きの省略形で, 実際は(quote ...)として解釈される(p.84の脚注34にも書いてる)
;; したがって以下は等価

;; ' は特殊形式なので"作用させる"とは言わない

    ''abracadabra
    (quote (quote abracadabra))

;; 最初のquoteを作用させた時点でそれ以降は手続きではなくデータとしてみなされる
;; つまり (quote abracadabra) というデータで, (x y) と同じように扱われるため,
;; (car '(x y)) がxであるように(car '(quote abracadabra)) はquoteとなる.


;; =============================================
;; p.85 2.3.2 記号微分 Symbolic Differentiation
;; =============================================

;; 次の規則を満たす微分プログラムを考える.
;; dc/dx = 0
;; dx/dx = 1
;; d(u+v)/dx =   du/dx  +   dv/dx
;; d(uv)/dx  = u(dv/dx) + v(du/dx)

;; 先に微分手続きderivの完成形を決める.
(define (deriv exp var) ;; 式(exp)を変数varで微分する
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) ;; d(u+v)/dx = du/dx + dv/dx
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
    (else
      (error "unknown expressions type -- deriv" exp))))


;; 代数式をリストで表現する
;; ===================================

;; ax + bをリストに落とす方法はいくつかある.
;;     表現法1. (a * x + b) : 中置記法
;;     表現法2. (+ (* a x) b) : 前置記法
;; ここでは, Lispの前置記法と親和性の高い(面倒の少ない) (2)を採用する.

;; (variable? e) symbolかどうか
(define (variable? x) (symbol? x))

;; (same-variable? v1 v2) 両者ともsymbolであり, かつeq?が成り立つかどうか
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; 和 ;;
;; (sum? e) リストの第一要素が+であればsum (前置記法だと判定が簡単)
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; (addend e) 加数 -- 和式listの第二項
(define (addend s) (cadr s))

; (augend e) 被加数 -- 和式listの第三項. 後に(q2.57で)任意長引数を扱うよう改良
(define (augend s) (caddr s))

; (make-sum a1 a2) -- (+ a1 a2)が印字される. より良い実装は後述
(define (make-sum a1 a2) (list '+ a1 a2))

;; 積 ;;
; (product? e) eは積か
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; (multiplier e)
(define (multiplier p) (cadr p))

; (multiplicand e) 後に(q2.57で)任意長引数を扱うよう改良
(define (multiplicand p) (caddr p))

; (make-product m1 m2).  より良い実装は後述
(define (make-product m1 m2) (list '* m1 m2))


;; ここまでの実装でderivは正しい答えを返すようになる.

    (deriv '(+ x 3) 'x)
    ;; => (+ 1 0)
    (deriv '(* x y) 'x)
    ;; => (+ (* x 0) (* 1 y))
    (deriv '(* (* x y) (+ x 3)) 'x)
    ;; => (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

;; 答えとして間違ってはいないが, 簡約化されていない(unsimplified).
;; 使いやすさのため, (x * 0) は 0になり式から取り除けることを知ってほしい.
;; derivをいじらず, make-sum と make-product に手を加える.

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum-better a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define make-sum make-sum-better)

(define (make-product-better m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define make-product make-product-better)

;; これを使うと, 印字される答えが簡潔になる.

    (deriv '(+ x 3) 'x)
    ;; => 1
    (deriv '(* x y) 'x)
    ;; => y
    (deriv '(* (* x y) (+ x 3)) 'x)
    ;; => (+ (* x y) (* y (+ x 3)))


;; q2.56
;; ---------------------------------------------

;; べき乗演算子の定義
(define (** b n) ;; bのn乗
  (cond ((= n 0) 1)
        ((= n 1) b)
    (else (* b (** b (- n 1))))))

    (** 2 10) ;; => 1024

;; べき乗を扱う選択子と構成子を定義する
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)     (cadr  x))
(define (exponent x) (caddr x))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1) ;; 1は何じょうしても1
    ((=number? exponent 0) 1) ;; 何かの0乗は1
        ((=number? exponent 1) base) ;; 何かの1乗はそれ自身
        ((and (number? base) (number? exponent)) (** base exponent)) ;; 計算可能なら計算
        (else (list '** base exponent))))

    (make-exponentiation 1 '(+ x y)) ;; => 1
    (make-exponentiation '(* x y) 1) ;; => (* x y)
    (make-exponentiation '(* x y) 0) ;; => 1
    (make-exponentiation 2 3) ;; => 8
    (make-exponentiation '(+ x y) 3) ;; => (** (+ x y) 3)
    (make-exponentiation '(+ x y) '(* x x)) ;; => (** (+ x y) (* x x))


;; exponentiation を追加したderiv
(define (deriv-with-expo exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-with-expo (addend exp) var)
                   (deriv-with-expo (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-with-expo (multiplicand exp) var))
           (make-product (deriv-with-expo (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
           (deriv-with-expo (base exp) var)))
        (else (error "unknown expressions type -- deriv-with-expo" exp))))



    ;; (x^2 + 1)' = 2x
    (deriv-with-expo '(+ (** x 2) 1) 'x)
    ;; => (* 2 x)

    ;; (x^3 + 2x^2)' = 3x^2 + 4x^2
    (deriv-with-expo '(+ (** x 3) (* 2 (** x 2))) 'x)
    ;; => (+ (* 3 (** x 2)) (* 2 (* 2 x)))
    ;; 2*2を計算してくれない(簡約化面の課題)


;; q2.57
;; ---------------------------------------------

;; derivを変更することなく引数3個以上も正しく扱えるようにしたい.

    (deriv '(* x y (+ x 3)) 'x)
    ;; => y (誤). 正解: 2xy + 3y

;; 第二項を返す選択子augendとmultiplicandを,
;; 第二項以降を返すように修正する.

    (augend '(+ x y z))
    ;; => y(誤). (+ y z) を返して欲しい
    (multiplicand '(* x y z))
    ;; => y(誤). (* y z) を返して欲しい

(define (augend-better s)
  (if (null? (cdddr s)) (caddr s) ;; (+ x y) で終わっているならyのみ返す
    (cons '+ (cddr s)))) ;; それ以外は第一項を覗いたものに+記号をconsして返す
(define augend augend-better)

(define (multiplicand-better s)
  (if (null? (cdddr s)) (caddr s)
    (cons '* (cddr s))))
(define multiplicand multiplicand-better)

    (deriv '(* x y (+ x 3)) 'x)
    ;; => (+ (* x y) (* y (+ x 3)))
    ;; 展開すると2xy+3yになる(簡約化面の課題)


;; q2.58
;; ---------------------------------------------

;; p.86 で前置記法を使うことを選択したが, 中置記法でも動かしたい.
;;     > ax + bをリストに落とす方法はいくつかある.
;;     >     表現法1. (a * x + b) : 中置記法
;;     >     表現法2. (+ (* a x) b) : 前置記法

;; a. 中置記法, ただし以下の簡略化を行ったもの
;;     * 和や積は常に二つの項を取るとする
;;     * 式は完全にカッコで囲まれているとする

    (x + (3 * (x + (y + 2))))

;; sum, product, exponentiationを再定義する.
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+))) ;; car -> cadr
; (addend e) 加数 -- listの第1項
(define (addend s) (car s))
; (augend e) 被加数 -- listの第三項(いまは二項の和のみ考慮すればよい)
(define (augend s) (caddr s))

;; makeする時の順序を変える.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2)) ;; 実際に足すときはSchemeに解釈させるため前置記法.
        (else (list a1 '+ a2)))) ;; (a1 + a2) とする.

;; +をquoteせず (list 1 + 2) とすると (1 #<subr +> 2) になるので注意.

;; 同様にproductとexponentiationを実装
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
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list base '** exponent))))

;; テスト

    (deriv '(x + (3 * (x + (y + 2)))) 'x)
    ;; => 4
    (deriv '(x * (y * (x + 3))) 'x)
    ;; => ((x * y) + (y * (x + 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; b. 中置記法, (a)で導入した簡略化を撤廃し普通の代数式と同じように書きたい.

;; *と+の優先度

    ;; 例:
    (x + 3 * (x + y + 2))

;; sum? product?等の手続きが, 要素の出て来る順番で判定できなくなるので,
;; 通常の代数式の定義を手続きに落としこむ.

;;;;; sum? 系... 要素の中に1つでも'+が存在すればsum式. ;;;;;
;; list中からitemを探し, 見つかったらそれ以降のlistを返すmemqを使う.
(define (sum? x)
  ;; (if (eq? #f (memq '+ x)) #f #t))
  (not (memq '+ x)))

    (sum? '(x + y + z)) ;; => #t
    (sum? '(x * y + z)) ;; => #t
    (sum? '(x * y * z)) ;; => #f

;; 加数addendは+より前の式を返す.
;; ただし, 要素数がひとつの時はリストではなく裸で返すようにする.
;; (この操作は繰り返し出てくるので手続きbareに切り出す)
(define (bare x)
  (if (null? (cdr x))
    (car x) ;; 要素が1個 (x) の場合は x を返す.
    x)) ;; それ以外はリストを返す.


(define (addend x)
  ;; (display x) (display " ->")
  (cond ((not (pair? x)) '())
        ((eq? (car x) '+) '()) ;; +に到達したらおしまい
        (else (bare (cons (car x)
                          (addend (cdr x))))))) ;; 再帰的に結合

    (addend '(x + y + z)) ;; => x
    (addend '(x * y + z)) ;; => (x * . y)
    (addend '(x * 3 * y + z * 3)) ;; => (x * 3 * . y)

;; 被加数augendは+より後ろの式を返す.
;; 基本的にはmemqを利用した上で +だけ除いてやればよい.

(define (augend x)
  (bare (cdr (memq '+ x))))

    (augend '(x + y + z)) ;; => (y + z)
    (augend '(x * y + z)) ;; => z
    (augend '(x * 3 * y + z * 3)) ;; => (z * 3)

;; make-sumはさっき(a)で定義した中置記法用の定義で問題ない.


;;;;; product?系...要素の中に+がなく(つまりsumではなく), かつ*が存在する. ;;;;;
(define (product? x)
  (if (and (not (sum? x))
           (has-*? x))
    #t #f))

(define (has-*? x)
  (if (eq? (memq '* x) #f) #f #t))

    (product? '(x * y)) ;; => #t
    (product? '(x * y * 2 * z)) ;; => #t
    (product? '(x + 1)) ;; => #f
    (product? '(x * y + 2)) ;; => #f

;; 中置記法なのでcarを取れば乗数.
(define (multiplier x) (car x))

    (multiplier '(x * y)) ;; => x
    (multiplier '(x * y * 2 * z)) ;; => x

;; 被乗数はaugendと同様にmemqで取れる.
(define (multiplicand x)
  (bare (cdr (memq '* x))))

    (multiplicand '(x * y)) ;; => y
    (multiplicand '(x * y * 2 * z)) ;; => (y * 2 * z)


;; ここまで定義すれば簡略化なし中置記法に対してderivが使える.

    (deriv '(x + 3 * (x + y + 2)) 'x)
    ;; => 4
    (deriv '(x * y * (x + 3)) 'x)
    ;; => ((x * y) + (y * (x + 3)))

