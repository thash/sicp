;; analyze-assignmentを追いかける

gosh> (definition? '(define x 2))
#t
gosh> (assignment-variable '(define x 2))
x
gosh> (assignment-value '(define x 2))
2
gosh> (analyze (assignment-value '(define x 2)))
#<closure (analyze-self-evaluating analyze-self-evaluating)>


gosh> the-global-environment
(((false true car cdr cons null? list memq member not and or + - * / > >= < <= = abs remainder integer? sqrt eq? display newline map set!) #f #t (primitive #<subr car>) (primitive #<subr cdr>) (primitive #<subr cons>) (primitive #<subr null?>) (primitive #<subr list>) (primitive #<subr memq>) (primitive #<subr member>) (primitive #<subr not>) (primitive #<syntax and>) (primitive #<syntax or>) (primitive #<subr +>) (primitive #<subr ->) (primitive #<subr *>) (primitive #<subr />) (primitive #<subr >>) (primitive #<subr >=>) (primitive #<subr <>) (primitive #<subr <=>) (primitive #<subr =>) (primitive #<subr abs>) (primitive #<subr remainder>) (primitive #<subr integer?>) (primitive #<closure sqrt>) (primitive #<subr eq?>) (primitive #<subr display>) (primitive #<subr newline>) (primitive #<subr map>) (primitive #<syntax set!>)))

;; 手続きの場合
gosh> (assignment-variable '(define (add x y) (+ x y)))
(add x y)
gosh> (assignment-value '(define (add x y) (+ x y)))
(+ x y)
gosh> (analyze (assignment-value '(define (add x y) (+ x y))))
#<closure (analyze-application analyze-application)>
