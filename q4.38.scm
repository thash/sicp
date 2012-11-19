;; multiple-dwelling手続きを修正し, SmithとFletcherが相隣る(?)階に住むものではないとの制限を除去する.
;; この修正パズルには何組の解が存在するか.
(load "./sec4.3-nondeterministic")

; (driver-loop)
;;; driver-loopを起動した後に定義する. >>> ココカラ
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (distinct? items)
  (if (null? items) true
    (if (member (car items) (cdr items))
      false
      (distinct? (cdr items)))))

;; multiple-dwelling に手を加える
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ; (require (not (= (abs (- smith fletcher)) 1))) SmithとFletcherが相隣る(?)階に住むものではないとの制限を除去する.
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;;; Amb-Eval input:
(multiple-dwelling)

;;; Starting a new problem

;;; Amb-Eval value
((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(multiple-dwelling)


;; したがって

((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5)) ; fとsが隣
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3)) ; fとsが隣
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3)) ; fとsが隣
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1)) ; fとsが隣

;; の5通りの解が存在する. 4通り可能性が増えた.
