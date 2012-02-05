(add-load-path ".")
(load "my_defs")

; use iterative cont-frac
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
            (/ (n i) (+ (d i) result)))))
  (trace iter)
  (iter k 0)
  )

(print (+ 2
  (cont-frac-iter
  (lambda (i) 1.0) ; n
  (lambda (i)      ; d
          (if (= 2 (remainder i 3))
          (* 2 (+ 1 (quotient i 3)))
          1))
  30)
  )
)


; answer from http://d.hatena.ne.jp/tanakaBox/20070714/1184424343
; (define (test f a b)
;   (define (iter i)
;     (cond ((<= i b)
;            (display (format "[~a] ~a\n" i (f i)))
;            (iter (+ i 1)))))
;   (iter a))
;
; (define (cont-frac2 n d k)
;   (define (iter i result)
;     (if (= i 0)
;         result
;         (iter (- i 1)
;               (/ (n i) (+ (d i) result)))))
;   (iter k 0))
;
; (test (lambda (k)
;         (+ 2
;            (cont-frac2 (lambda (i) 1.0)
;                    (lambda (i)
;                      (if (= (remainder 3 i) 2)
;                          i
;                          1))
;                    k)))
;       1 100)

