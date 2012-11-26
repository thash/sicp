(load "./sec4.3-nondeterministic")
;; 失敗の時にもやり直さない permanent-set! という新しい種類の代入を実装せよ.
;; 例えば次のように, リストから2つの異なる要素を選び, 成功した選択ができるまでに必要な試行数が得られる.
;;; require定義(driver-loop中) {{{
(define (require p)
  (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
;;; }}} 前提定義
(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;; 実装 ;;
;; 1. analyzeのcond節にpermanent-set!判定を追加
;; 2. permanent-set?を定義
;; 3. analyze-permanent-set!を定義
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-set? exp) (analyze-permanent-set! exp)) ;; 追加
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

(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-set! exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok
                        (lambda () ;; 失敗継続でold-valueへ戻さない.
                          (fail2))))
             fail))))

;; 動作確認
;; 4.3load -> 上のヤツ定義 -> driver-loop -> requireとか定義 -> count部分実行 -> try-again

;;; Starting a new problem
;;; Amb-Eval value
(a b 2)
(a c 3) ;; try-again
(b a 4) ;; try-again
(b c 6) ;; try-again
(c a 7) ;; try-again
(c b 8) ;; try-again
;;; There are no more values of
(let ((x (an-element-of '(a b c))) (y (an-element-of '(a b c)))) (permanent-set! count (+ count 1)) (require (not (eq? x y))) (list x y count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ここでpermanent-set!の代わりにset!を使ったらどうなるか.
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1)) ;; ここを変更
  (require (not (eq? x y)))
  (list x y count))

;;; Amb-Eval input:
;;; Amb-Eval value
(a b 1)
(a c 1) ;; try-again
(b a 1) ;; try-again
(b c 1) ;; try-again
(c a 1) ;; try-again
(c b 1) ;; try-again
;;; There are no more values of
(let ((x (an-element-of '(a b c))) (y (an-element-of '(a b c)))) (set! count (+ count 1)) (require (not (eq? x y))) (list x y count))


