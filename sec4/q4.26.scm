;; "unlessのようなもの"をどう実装すべきか論

;; Ben < unlessは作用的順序でも特殊形式で実装できる
;; Alyssa < それだとunlessは手続きではなく単なる構文になっちゃう

;; (1). unlessを(condやletのように)導出された式(derived expression)として実装する方法.
(define (eval exp env)
  ;...
  (cond ;....
        ((unless? exp) (eval (unless->if exp) env))
        ;...
        )
  ;...
  )

;; ifを参考に
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

;; make-ifに対してconsequentとalternativeを逆にして渡してやる
(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))


;; (2). unlessが特殊形式ではなく手続きとして使えると嬉しい実例を述べよ.
;; http://wqzhang.wordpress.com/2009/11/29/sicp-exercise-4-26/
;; こんなのが使えるからだってさ.

(define (foo f condition value-1 value-2)
  (f condition value-1 value-2))

(foo unless (> 2 10) 1 2)
; Unbound variable unless
;   because unless is a special form

;; procedureとして定義していればok
(define (unless-proc condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(foo unless-proc (> 2 10) 1 2)
; 1

