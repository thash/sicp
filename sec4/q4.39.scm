;; multiple-dwelling手続きの式順序は結果に影響を与えるか?
(load "./sec4.3-nondeterministic")


;(driver-loop)
;;; driver-loopを起動した後に定義する. >>> ココカラ
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;; オリジナル
; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; 改変版
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (> miller cooper)) ;; これをdistinct?の次に持ってきてみた
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

; ;;; Amb-Eval value
; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; N=1だけど同じの気配. このへんでネット見る

;; 「同じ」 = 「順序に左右されない」
;; 計算に失敗する場合が多い条件を先に持ってくると速くなる.
;; 最初に1,2,3,4,5が5個, 5**5を生成してしまっている. ここですべての割り当てを生成する前に逐一検査していけば良い. というのがq4.40.scmか

