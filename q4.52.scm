(load "./sec4.3-nondeterministic")
;; 式の失敗を捉えることができる if-fail を実装せよ.

;;;; 使い方 ;;;;
;; if-fail は2つの式を取る. 第一の式を通常に評価し,
;;                            評価が成功すれば通常に戻る.
;;                            評価が失敗すれば, 次の例のように第二の式が戻される:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
           'all-odd)
;; => 評価すると all-odd が返る.

(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;; => 評価すると 8 が返る.

;; 実装 ;;
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp)) ;; 追加
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-success exp) (cadr exp))
(define (if-fail-failure exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-fail-success exp)))
        (fproc (analyze (if-fail-failure exp))))
    (lambda (env succeed fail)
      (sproc env
             succeed
             (lambda ()
               (fproc env succeed fail))))))

;; driver-loop後以下を定義
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; 上のテストを実行.

