;; 2.5.3. 例: 記号代数
;;   代数式は階層構造（被演算子に作用させる演算子の木構造）と見ることが出来る。
;;   定数や変数を、加算や乗算といった代数演算子で組み合わせる。
;;
;;   多項式はある変数 - 不定元(indeterminates)に対して定義される。一番簡単なのは一元多項式(unvariate polynomials).
;;   多項式を、変数と項の集まりからなる多項式型(poly)で表現する。

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (error "Polys not in same var -- ADD-POLY"
           (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))
    (error "Polys not in same var -- MUL-POLY"
           (list p1 p2))))
  

;; 汎用算術演算システムに加えるために、型タグ"polynomial"を使う.

(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; 2.3.2節の手続きsame-variable?とvariable?を使います、と。

  ;; 項と項リストの表現
  ;; 以下の本文にある手続き adoin-term ... coeff
  (define (add-poly p1 p2) ...)
  ;; add-poly が使う手続き
  (define (mul-poly p1 p2) ...)
  ;; mul-poly が使う手続き

  ;; システムの他の部分とのインターフェイス
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make '(polynomial polynomial)
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

;; TODO: skip transcription here

;; memo...
;;
;;  coeff: 係数
;;  多項式は次数の順に並んでいるという前提？ そんなことなさげか。
;;  addの対象がpolynomialになる場合もある。
;;  addが項の係数を足す(こともあれば、単純に数を足すこともある? かな)
;;
;;  汎用手続きaddとmulによって、前節で作った複素数やら有理数を含んだ数式、柔軟に拡張できる。


;; 項リストの表現
;;   多項式の濃い/薄い. x^100 + x^2 + 1とかは薄いよ、という話。
;;   濃いときは(1 2 0 3)みたいに係数のリストを作れるが、薄いときは0だらけになる。
;;   そこで、((100 1) (2 2) (0 1))というリストで表せば効率的。





