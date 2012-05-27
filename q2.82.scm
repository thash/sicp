(load "./q2.81") ;; includes 2.4.2, 2.4.3, 2.5.1, 3.3.3, q2.77, sec2.5.2

; apply-genericを argsが3個以上の場合にも対応させる
;   1つの戦略はすべての引数を第一引数の型に揃え、それでだめなら第二引数...と順々に強制変換を試みること

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        ; これまでは (if (= (length args) 2) で引数が2個の時のみ適用していた
        (define (find-coercion type-tags)
          (let ((type1 (car type-tags))
                (other-types (cdr type-tags))
            (if (null? other-types)
              #f
              (let ((coercion-proc (get-coercion (car type-tags) type1))
              (find-coercion (cdr type-tags)))
              )))))))))

;        (if (= (length args) 2)
;          (let ((type1 (car type-tags))
;                (type2 (cadr type-tags))
;                (a1 (car args))
;                (a2 (cadr args)))
;            (if (eq? type1 type2)
;              (and (display "[Same type] Error: No method for these types")
;                   (display (list op type-tags)))
;            (let ((t1->t2 (get-coercion type1 type2))
;                  (t2->t1 (get-coercion type2 type1)))
;              (cond (t1->t2
;                      (apply-generic op (t1->t2 a1) a2))
;                    (t2->t1
;                      (apply-generic op a1 (t2->t1 a2)))
;                    (else
;                      (and (display "Error: No method for these types")
;                           (display (list op type-tags)))))))
;          (and (display "Error: No method for these types")
;               (display (list op type-tags)))))))))


