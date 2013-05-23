;; 評価機を拡張し, cond. letなどのような導出された式(derived expression)(sec4.1.2)が扱えるようにせよ.
;; cond->ifのような構文変換器が, 機械演算として使用可能なように「欺き」また仮定してよい.

;; ラベル2重定義した時は先に定義した方が有効になる(ref:q5.8.scm)ため
;; 普通のSchemeのような上書き手続きは不可能. sec5.4の該当ラベルをまるまる書き換えてやる.

eval-dispatch
(test (op self-evaluating?) (reg exp))
(branch (label ev-self-eval))
(test (op variable?) (reg exp))
(branch (label ev-variable))
(test (op quoted?) (reg exp))
(branch (label ev-quoted))
(test (op assignment?) (reg exp))
(branch (label ev-assignment))
(test (op definition?) (reg exp))
(branch (label ev-definition))
(test (op if?) (reg exp))
(branch (label ev-if))
(test (op let?) (reg exp)) ; 追加
(branch (label ev-let)) ; 追加
(test (op lambda?) (reg exp))
(branch (label ev-lambda))
(test (op begin?) (reg exp))
(branch (label ev-begin))
(test (op cond?) (reg exp)) ; 追加
(branch (label ev-cond)) ; 追加
(test (op application?) (reg exp))
(branch (label ev-application))
(goto (label unknown-expression-type))


ev-let
(assign exp (op let->combination) (reg exp))
(goto (label eval-dispatch))

ev-cond
(assign exp (op cond->if) (reg exp))
(goto (label eval-dispatch))

;;
(define eceval-operations
  ;; ...
  (let? ,let?)
  (let->combination ,let->combination)
  (cond? ,cond?)
  (cond->if ,cond->if)
  ;; ...
  )


;; 動作テスト
(define (myabs n)
  (cond ((< n 0) (- n))
        ((= n 0) n)
        ((> n 0) n)
        (else (error "unknwon" n))))

(define (lettest a b)
  (let ((x (+ a b))
        (y (- a b)))
    (* x y)))
