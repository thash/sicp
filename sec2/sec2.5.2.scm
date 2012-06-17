;; 2.5.2. 異なる型のデータの統合
;;
;;   いままで定義した型は完全に独立であったが、
;;   この節では「複素数を通常の数に足す」というような型の境界を越える演算を考える。
(load "./q2.77") ;; includes 2.4.2, 2.4.3, 2.5.1, 3.3.3.

;;   1つの方法: 演算する型の組み合わせ事に別の手続きを設計する

(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; 問題点: 拡張に弱い。型間の演算定義が、型自体の定義の数よりも多くなる。
;;         型A-型B間の演算はどちらのパッケージに含めるべきか、一貫した基準が持てない。

;; 【強制型変換】
;; 例: scheme-numberからcomplexへの変換
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; 強制型変換表に格納する
;;   本文中には記述がないが, get/put-coercionを利用するために強制型変換テーブルを定義
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;; 逆に complex->scheme-number と変換する方法は(虚部が0の時を除き)存在しない。
;; 強制型変換表は一般に空で、たまたま変換可能な方向のみ手続き名が入っているイメージ

;; 強制型変換を扱う拡張版apply-genericを作る. 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args)) ; contentsは何？
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args))) ; memo. q2.81.scm (c) で改良
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else
                      (and (display "Error: No method for these types")
                           (display (list op type-tags)))))))
          (and (display "Error: No method for these types") ; beginとかあったっけ
               (display (list op type-tags))))))))

;; 対応する演算をprocに格納。procが存在すればそのまま実行、存在しなければ強制型変換を試みる。
;; t1->t2かt2->t1のどちらかが存在すればいい
;; A->Cの変換を知らなくてもA->BとB->CがわかっていればA->B->Cと変換できる、という利点もある

;; 【型の階層構造(hierarchy of types)】
;; 例: 整数 ⊆ 有理数 ⊆ 実数 ⊆ 複素数

