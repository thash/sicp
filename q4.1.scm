;; sec4.1-The-Metacircular-Evaluator.scm のlist-of-valuesが被演算子を左右どっちから評価するかは評価系のconsに依存している.
;; 評価系のconsがどちらであろうとも, 必ず左から(or 右から)評価するようなlist-of-valuesを作れ.
(load "./sec4.1-The-Metacircular-Evaluator")

;; evalは動いているか
;     gosh> (define a 1)
;     gosh> (eval a '())
;     1
;; ok

;; なつたんのサンプル. 第二引数はダミー
(define (eval-print x y)
  (debug-print x)
  x)

;; 第二引数はダミー
(cons (eval-print 'a '()) (eval-print 'b '()))

;; 回答
(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
    '()
    (let ((rhs (list-of-values-r2l (rest-operands exps) env)))
      (cons (begin (display (first-operand exps)) ;; ここのbegin+displayは評価順をチェックするためのもの.
                   (first-operand exps))
            rhs))))

; gosh> (list-of-values-r2l '(1 2 3) '())
; 321(1 2 3)

(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
    '()
    (cons (begin (display (first-operand exps))
                 (eval (first-operand exps) env))
          (list-of-values-l2r (rest-operands exps) env))))

; gosh> (list-of-values-l2r '(1 2 3) '())
; 123(1 2 3)

;; このlist-of-values-l2rは右から左に評価するconsに対応してない気がする


;; 参考にする -> http://sicp.g.hatena.ne.jp/yad-EL/20080305/1204703488

(define the-global-environment (setup-environment))

(list-of-values
  '((begin (set! (+ x 3)) (newline) (display x))
    (begin (set! (+ x 2)) (newline) (display x))))


