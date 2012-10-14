;; let* を実装せよ.
(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))
;; これはletで定義した順に次々見えていて, letの入れ子にする必要がない構文だった.
;; 逆に考えると, 処理系としてはletの入れ子に書き換えてやればいいわけだ.

(define (eval exp env)
  ;; ...
  ((let*? exp) (eval (let*->nested-lets exp) env))
  ;;...
  )

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-assignment exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (transform-let* assignment body)
  (if (null? (cdr assignment))
    (cons 'let (cons (assignment body))
          (list 'let (list (car assignment))
                (transform-let* (cdr assignment) body)))))

(define (let*->nested-lets exp)
  (transform-let* (let*-assignment exp) (let*-body exp)))

