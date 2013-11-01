;;問題: 汎用算術演算パッケージに多項式用の=zero?を定義せよ。これはadjoin-termが、その係数自体がまた多項式である多項式に対しても働くものとする。
;;
;; つまり, (y^2 + 1)x^2 + (0y^2 + 0y)x + 1のような式に対しても, xの係数が=zero? trueになるということ
;; 内部手続き=zero系を幾つか定義して, 最後に=zero?へ統合, polynomialにdispatch.
;;
;; (every =zero? list) everyは、listがすべてzeroかどうかを判定する手続き。
;;                    Rubyで言うEnumerable#allか。
;;
;; 多項式相手にも使えるように。
;; (make-polynomial .. 'x '((5 0) (4 0)))は=zero?が#tだけど'((5 1) (4 0))とすると#fになるような。

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

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

  ;; here, L = (x ((1 2) (4 5)))
  ;; (first-term L) = x... これか。
  ;; (=zero? (coeff (first-term L))) ... これが素numberに対して適用できてない。
  ;;  => 最終的にapply-numberへのdrop適用がうまくいってないことが問題だった。q2.85のapply-genericへのdrop適用がうまくいってなくてそれでエラってた。
  (define (=zero-terms? L)
    (or (empty-termlist? L)
        (and (=zero? (coeff (first-term L)))
             (=zero-terms? (rest-terms L)))))
 ; (define (=zero-poly? p)
 ;   (=zero-term? (term-list p)))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  ;; (put '=zero? '(polynomial)
  ;;      (lambda (p) (=zero-poly? p)))
  ;; (put '=zero? '(polynomial)
  ;;      (lambda (p) (tag (=zero-poly? p))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (=zero-terms? (term-list p))))

  'done)

;(define (=zero? p)
;  ((get '=zero? '(polynomial)) p))

(define (=zero? x) (apply-generic '=zero? x))

