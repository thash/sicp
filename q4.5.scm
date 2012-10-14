;; 裏condを実装せよ.
;;     (<test> => <recipient>)
;; <test>が真の値に評価されると<recipient>>が評価される.

;; 例
(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else #f))

;; expand-clausesはcondの評価にのみ使われている.
;; clausesには, (cond のみが除かれた条件と式のペアリストがずらずらと渡ってくる.
;; expand-clauses内, make-ifに渡す引数を変えるため, cond-actionsを調整/拡張する.

;; 元
;(define (cond-actions clause) (cdr clause))

;; => 形式かどうかを判定する
(define (extended-cond? clause)
  (if (cond-else-clause? clause)
    #f
    (eq? (cadr clause) '=>)))

;; => 形式の時は順序をいじって返せばよい
(define (cond-actions clause)
  (if (extended-cond? clause)
    (list (caddr clause) (car clause))
    (cdr clause)))


