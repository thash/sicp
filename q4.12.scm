;; set-variable-value!, define-variable!, lookup-variable-value
;; の共通部分を抽象化せよ

;; in lookup-variable-value
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop (enclosing-environment env)))
;            ((eq? var (car vars))
;             (car vals)) ;; <=
;            (else (scan (cdr vars) (cdr vals)))))

;; in set-variable-value!
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop (enclosing-environment env)))
;            ((eq? var (car vars))
;             (set-car! vals val)) ;; <== 上の環境にあるvalを代入
;            (else (scan (cdr vars) (cdr vals)))))

;; in define-variable!
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (add-binding-to-frame! var val frame)) ;; <= null の時の処理が違う
;            ((eq? var (car vars))
;             (set-car! vals val))
;            (else (scan (cdr vars) (cdr vals)))))


;; varとprocを渡せるようにして抽象化したscan
;; 元のscanではenv-loopかadd-binding-to-frame!を呼んでいたが,
;; 単に#fを返して, nullの時の処理を呼び出す側に任せる.
(define (scan var vars vals proc)
  (cond ((null? vars) #f)
        ((eq? var (car vars))
         (proc vars vals))
        (else (scan var (cdr vars) (cdr vals) proc))))

;; env(= framesの層)を走査して, 見つかれば(scanの返す)proc実行結果, 見つからなければ#fを返す
;; 見つからなければfound-pairは#fになる
(define (env-loop var env proc)
  (if (eq? env the-empty-environment)
    #f
    (let ((frame (first-frame env)))
      (let ((found-pair
              (scan var (frame-variables frame) (frame-values frame) proc)))
        (if (not (eq? #f found-pair))
          found-pair
          (env-loop var (enclosing-environment env) proc))))))

;; 以上を使って再定義.
;; env-loopでenv(=全frame)を走査した結果をvarに格納し, 見つかったかどうかで処理を分ける
;; (if (eq? #f val)) は (if (not val)) でもいいけど可読性のため.
(define (lookup-variable-value var env)
  (let ((val (env-loop var env
                       (lambda (vars vals) (car vals)))))
    (if (eq? #f val)
      (error "Unbound variable" var)
      val)))

(define (set-variable-value! var val env)
  (let1 val (env-loop var env
                      (lambda (vars vals) (set-car! vals val)))
        (if (eq? #f val)
          (error "Unbound variable -- SET!" var)
          val)))

;; こいつはenvのfirst-frameのみscanし, 存在しなかった場合のみ束縛を行う.
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((result (scan var
                        (frame-variables frame)
                        (frame-values frame)
                        (lambda (vars vals) (set-car! vals val) #t))))
      (if (eq? #f result)
        (add-binding-to-frame! var val frame)))))


