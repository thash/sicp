;; (1). 分岐を左から右ではなくランダムな順に探す ramb を実装せよ.
(use srfi-27)
(define (random x) (random-integer x))
;; (random 3) で 0,1,2 のいずれかが返る

;; [没] itemsからランダムに一つ取ってくる手続き {{{
;; * ref: http://d.hatena.ne.jp/rui314/20070118/p1
;; * ref: http://practical-scheme.net/gauche/man/gauche-refj_44.html
(define (pick1 items)
  (if (null? items) #f
    (list-ref items (random (length items)))))
;; ...を考えたが, 1つ取った後にそれ以外のリストを使って失敗継続に渡さないといけないのでこれではだめ. }}}

;; ambをrambで置き換える.
;; analyze-amb -> analyze-ramb

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
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))


;; (2). これが問題4.49 (q4.49.scm) のAlyssaの問題を救うことを示せ.

