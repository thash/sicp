(load "./sec2.5.2") ;; includes 2.4.2, 2.4.3, 2.5.1, 3.3.3, q2.77

; Louisはapplly-genericは引数が既に同じ型を持っていても、互いの型へ強制変換を試みるべきだと考えた。
; これを実現するために自分自身の型へ強制変換する手続きを定義する必要がある。
(define (install-q2.81-selfcoercion)

  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex))

; (a). Louisの強制型変換手続きが設定されると、applly-genericが型scheme-numberの2つの引数やcomplexの2つの引数で、これらの型で表に見つからない手続きに対して呼び出されると、何が起きるか。例えば汎用べき乗演算:

(define (exp x y) (apply-generic 'exp x y))

; が定義してあり、scheme数パッケージのべき乗の手続きは存在するが他にはないものとしよう。
; 引数に2つの複素数を持ってexpを呼び出すと何が起きるか。

; 実際にやってみた。 => q2.81-test.scm

;; sec2.5.2.scm で定義したapply-generic では、
;; 演算表を引き、「procが存在すればそのまま実行、存在しなければ強制型変換を試みる」ため、
;; 強制型変換表に同じ型への変換が定義されていると無限ループになる。

; (b). ↑の理由により正しくない。
; (c). 2つの引数が同じ型のとき強制型変換を試みないように sec2.5.2.scm の applly-genericを修正せよ.

; apply-generic2
(define (apply-generic2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (eq? type1 type2)
              (and (display "[Same type] Error: No method for these types")
                   (display (list op type-tags)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic2 op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic2 op a1 (t2->t1 a2)))
                    (else
                      (and (display "Error: No method for these types")
                           (display (list op type-tags)))))))
          (and (display "Error: No method for these types")
               (display (list op type-tags)))))))))




