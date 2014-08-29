(let ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)
;; は
((lambda (<var1> ... <varn>)
   <body>)
   <exp1>
   ...
   <expn>)
;; に等価である.

;; したがってlet式は導出された式である.
;;     # 導出された式(derived expression) <=> 特殊形式
;; let->combination を実装し, let式が使えるようにせよ.

;; http://wqzhang.wordpress.com/2009/10/12/sicp-exercise-4-6/

;; 下準備はこんな感じ
(define (eval exp env)
  ;;...
  ((let?  exp) (eval (let->combination exp) env))
  ;;...
  )

(define (let? exp) (tagged-list? exp 'let))

;; carとかcdrを文脈に応じてwrapする
(define (let-assignment exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-exp assignment)
  (if (null? assignment)
    '()
    (cons (cadr (car assignment))
          (let-exp (cdr assignment)))))

(define (let-var assignment)
  (if (null? assignment)
    '()
    (cons (car (car assignment))
          (let-var (cdr assignment)))))

(define (transform-let assignment body)
  (cons (make-lambda (let-var assignment) body)
        (let-exp assignment)))

(define (let->combination exp)
  (transform-let (let-assignment exp) (let-body exp)))
