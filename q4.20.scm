;; 内部定義は逐次のように見えて実は同時なので(Evaの立場? どこを前提にした"なので"なのか)
;; letrec (定義内で自分自身の名前を参照できる)を使うほうが良い, という主張.
;; http://d.hatena.ne.jp/higepon/20080625/1214407271
;; http://practical-scheme.net/wiliki/wiliki.cgi?R6RS%3a翻訳%3aR6RS%3a11.4.6%20Binding%20constructs

(define (f x)
  (letrec ((even?
             (lambda (n)
               (if (= n 0)
                 #t
                 (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (= n 0)
                 #f
                 (even? (- n 1))))))
    <rest_of_f>))


;; (a). letrecをlet式に変換し, 導出された式として実装せよ.
;; http://wqzhang.wordpress.com/2009/11/25/sicp-exercise-4-20/
(define (eval exp env)
  ;;...
  ((letrec? exp) (eval (letrec->let exp) env))
  ;;...
  )

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (make-unassigned-letrec vars)
  (if (null? vars)
    '()
    (cons (list (car vars) ''*unassigned*)
          (make-unassigned-letrec (cdr vars)))))
(define (make-set-letrec vars exps)
  (if (null? vars)
    '()
    (cons (list 'set! (car vars) (car exps))
          (make-set-letrec (cdr vars) (cdr exps)))))
(define (letrec->let exp)
  (let* ((assi (let-assignment exp))
         (lvars (let-var assi))
         (lexps (let-exp assi)))
    (cons 'let (cons (make-unassigned-letrec lvars)
                     (append (make-set-letrec lvars lexps)
                             (let-body exp))))))

;; (b). Louis 曰く "手続きの内側でdefineが使いたくなければletを使うことができる"
;; 環境図を書け. んでもってLouisの間違いを示せ, と

;; http://d.hatena.ne.jp/tmurata/20100326/1269618206
;; letrecがletになったとすると, こうなる.
(define (f x)
        (let ((even? '*unsigned*)
              (odd? '*unsigned*))
          (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
          (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
          <rest_of_f>))

;; 一方Louisが言う "defineの代わりにlet" を書き下すと
(define (f x)
  (let ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
        (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
    <rest_of_f>))

;; これだとエラーになるそうな

;; たとえばfactを考えよう. defineして実行.
;; 周りのlambdaは即時実行させたいだけだから気にしない.
((lambda (x)
   (define (fact n)
     (if (= n 1)
       1
       (* n (fact (- n 1)))))
(fact x)) 5)
;; => 120 ... ok.

;; Louis「defineはletで置き換えられる！」
;; いいぜ まずはそのふざけた幻想をぶち壊す
((lambda (x)
  (let ((fact (lambda (n)
                (if (= n 1)
                  1
                  (* n (fact (- n 1))))))))
  (fact x)) 5)
;; =>  !!!*** ERROR: unbound variable: fact!!!

;; 何となれば, 最初にfactを定義しようとしているのに再帰的にfactを参照しているから.
;; つーかむしろdefineが再帰的定義できるのは何なんだ

