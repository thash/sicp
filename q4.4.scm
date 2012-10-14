;; andとorを評価系組み込みのものを使うのではなく, 特殊形式として定義.
;; つまりeval-assignmentとかeval-definitionと同じノリでeval-andとeval-orを作ってやる.
;; http://wqzhang.wordpress.com/2009/09/17/sicp-exercise-4-4/

;; 下準備はこんな感じ
(define (eval exp env)
  ;;...
  ((and? exp) (eval-and exp env))
  ((or?  exp) (eval-or  exp env))
  ;;...
  )

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

;; 問題の中身.
;; cdrとかじゃなくてrest-operandsとかwrapされたもの使ったほうがいいよね, っと.
(define (eval-and exp env)
  (define (iter operands)
    (cond ((null? operands) #t) ;;最後まで耐えきれればtrue
          ((true? (eval (first-operand operands) env))
           (iter (rest-operands operands)))
          (else #f)))
  (iter (operands exp)))

(define (eval-or exp env)
  (define (iter operors)
    (cond ((null? operors) #f)
          ((true? (eval (first-operor operors) env))
           #t)
          (else (iter (rest-operors operors)))))
  (iter (operors exp)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 導出された式 (= derived expression) としてのand/or


(define (eval exp env)
  ;;...
  ((and? exp) (eval (and->if exp) env))
  ((or?  exp) (eval (or->if  exp) env))
  ;;...
  )

;; コレの親玉であるexpand-clauses は何やってるのか. clause = "節".
;; ここでは生true/falseではなくevalされる表現として'true/'falseを返してる.
(define (expand-and-clauses clauses)
  (if (null? clauses)
    'true
    (make-if (car clauses)
             (expand-and-clauses (cdr clauses))
             'false)))
(define (and->if exp)
  (expand-and-clauses (cdr exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
    'false
    (make-if (car clauses)
             'true
             (expand-or-clauses (cdr clauses)))))
(define (or->if exp)
  (expand-or-clauses (cdr exp)))

