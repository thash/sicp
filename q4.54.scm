(load "./sec4.3-nondeterministic")
;; 利用者が非決定性プロブラムの一部として定義するrequireが, ambを扱う通常の手続きとして実装できると気づかなかったら,
;; これを特殊形式として実装したであろう. これには構文手続き
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))
;; と, analyzeの新しい節
((require? exp) (analyze-require exp))
;; とrequire式を扱う手続きanalyze-requireが必要である. analyze-requireの定義を完成せよ.
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if <??>
                   <??>
                   (succeed 'ok fail2)))
             fail))))

;;;;; <<<< 問題文ココマデ

;;; 実装 ;;;
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((require? exp) (analyze-require exp)) ;; 追加
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

;;; 参考: ambを使ったrequire
; (define (require p)
;   (if (not p) (amb)))
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (eq? false pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

;; loadしたあと実装を読み込み, driver-loop.
;; driver-loop 後にan-element-ofを定義
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; テスト
(an-element-of '(1 2 3 4 5))


