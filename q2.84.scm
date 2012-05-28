;; raiseをapply-genericに組み込む
;; 引数を同じ型になるまで順次高めて行く

(define tower '(integer rational scheme-number complex))

;; 二つの型のうち塔の中で高い方を返す
(define (higher type1 type2)
  (let ((type1-pos (memq type1 tower))
        (type2-pos (memq type2 tower)))
    (if (or (eq? #f type1-pos) (eq? #f type2-pos))
      (error "Error: invalid type(s)." type1 type2) ; type1, 2いずれかがtowerに含まれていないときエラー

      ; memq返り値のlengthが小さい方がレベル高い
      (cond ((= (length type1-pos) (length type2-pos)) type1) ; 同じならどっちでもいいけどtype1を返す
            ((< (length type1-pos) (length type2-pos)) type1)
            ((> (length type1-pos) (length type2-pos)) type2)))))

(define (raise-to n target-type)
  (if (eq? (type-tag n) target-type)
    n
    (raise-to (raise n) target-type)))

(define (apply-generic op . args)
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
              (error "[Same type] Error: No method for these types" op type-tags)
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "Error: No method for these types" op type-tags)))))
            (error "Error: No method for these types" op type-tags)))))))



