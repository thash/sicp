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
  ;; 内部手続き -- 多項式型の表現
  ;;   variableとは変数、つまりxのことで、基本的に何でもいい。
  ;;   term-listが((乗数 係数) (乗数 係数) (乗数 係数)...)を意味して、
  ;;   termは1つの(乗数 係数)であり、
  ;;   (order term) = (car term) = 乗数
  ;;   (coeff term) = (cdr term) = 係数

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; 2.3.2節の手続きsame-variable?とvariable?を使います、と。
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; 項と項リストの表現
  ;; add-poly, mul-polyは以下を参照した
  ;;   ref. http://d.hatena.ne.jp/rsakamot/20090604/1244083653
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same variable -- ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same variable -- MUL-POLY"
             (list p1 p2))))

  ;; システムの他の部分とのインターフェイス
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

;; memo...
;; 多項式は次数の順に並んでいるという前提？ そんなことなさげか。
;; addの対象がpolynomialになる場合もある。
;; addが項の係数を足す(こともあれば、単純に数を足すこともある? かな)
;;
;; coeff(係数), order(次数)を返す選択肢が定義されているとする。

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1)
                                 (rest-terms L2)))))))))

;; ここで項の係数を足すために汎用手続きaddを使った。
;;  汎用手続きaddとmulによって、前節で作った複素数やら有理数を含んだ数式、柔軟に拡張できる。

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t2 (first-term L)))
      (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))


;; 【項リストの表現】
;;   多項式の密度が濃い/薄い. x^100 + x^2 + 1とかは薄いよ, という話.
;;   濃いときは(1 2 0 3)みたいに係数のリストを作れるが、薄いときは0だらけになる.
;;   そこで ((100 1) (2 2) (0 1)) というリストで表せば効率的.

;; 名前だけは登場していた補助手続きを実装する.
;;   後半は文脈と読みやすさのために再定義した程度の意義

(define (adjoin-term term term-list)
  (if (=zero? (coeff term)) ;; 0 なら追加しない.
    term-list
    (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff)) ;; 3x^2項は(2 3)と表される。
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

