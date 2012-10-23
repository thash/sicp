;; 内部定義を解釈するためにunassigned方式を採る. letが使えると過程.

;; (a). lookup-variable-valueを変更し, 見つけた値が*unassigned*ならエラーにする
(load "./sec4") ;; letを書き加えた.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            ((eq? var '*unassigned*) ;; scan中のcond節に追加
             (error "Unassigned variable" var))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;; 内部定義(二重define)をletに展開する処理をひとつの手続きにしてしまう.
;; マクロじゃだめですか.
(define (scan-out-defines p)
  ())

