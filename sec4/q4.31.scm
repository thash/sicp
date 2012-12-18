;; 上位互換拡張(upward-compatible extension)として遅延評価を実装する.
;; つまりそれぞれの引数について
;;   * その場評価
;;   * 遅延評価
;;   * 遅延評価+メモ化
;; の中から選択可能にする.

;; http://sioramen.sub.jp/blog/2008/02/sicp-422.html ここを元に写経.

;; 仮引数にリスト形式でflagを渡すことで中の処理を分岐できなければならない.
(define (f x (y lazy))
  (+ x y))

;; 元々のprocedureはこんな形
(procedure (x y) ((+ x y)) the-global-environment)
;; これを以下のように変える.
;; (() lazy) というS式が追加されている. list要素にflag(modifier)のlistが加わっているのである.
(procedure (x y) (() lazy) ((+ x y)) the-global-environment)


;; original make-procedure
;; (define (make-procedure parameters body env)
;;   (list 'procedure parameters body env))

(define (make-procedure parameters body env)
  ;; unmodify ... (x lazy) -> x
  (define (unmodify a)
    (if (symbol? a) a (car a)))
  ;; take and save modifier (lazy or lazy-memo).
  (define (modifier a)
    (if (symbol? a) '() (cadr a)))

  (let ((p (map unmodify parameters)) ;;  <- (x y)
        (m (map modifier parameters))) ;; <- (() lazy)
    (list 'procedure p m body env)))

;; 要素が増えるのでparametersより後ろはcdrが増える.
; (define (procedure-parameters p) (cadr p)) <- そのまま
(define (procedure-body p) (cadddr p))
(define (procedure-environment p) (car (cddddr p)))

;; modifier listも取りたい.
(define (procedure-modifier p) (caddr p))



(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             ;; ここが変わる.
             (list-of-delayed-args
               arguments
               (procedure-modifier procedure)
               env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (delay-memo-it exp env)
  (list 'thunk-memo exp env))
(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (list-of-delayed-args exps modifier env) ;; 引数にmodifierが加わった
  (if (no-operands? exps)    '()
    ;; modifierとexpsをletで変数に入れつつ,
    (let ((m (first-operand modifier))
          (o (first-operand exps)))
      ;; 一律delay-itしてた部分をmodifier見て条件分岐する. modifierがカラなら即評価.
      (cons (cond ((eq? m 'lazy) (delay-it o env))
                  ((eq? m 'lazy-memo) (delay-memo-it o env))
                  ((null? m) (actual-value o env))
                  (else (error "Unknown modifier -- LIST-OF-DELAYED-ARGS" m)))
            (list-of-delayed-args (rest-operands exps)
                                  env)))))

;; forceするときの扱いがちょっと分岐入る. メモも遅延も選択可能なversion.3です.
(define (force-it obj)
  (cond ((thunk-memo? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ;; 評価済tagがついてる = メモあり引数として渡されている.
        ((evaluated-thunk? obj) (thunk-value obj))
        ;; ここまでcondで落ちて来なかったけどthunkだけ指定されてた => いま評価
        ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))


