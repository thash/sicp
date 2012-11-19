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

