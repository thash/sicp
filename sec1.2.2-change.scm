; back here from q2.19 -- charge with list
; 1ドルの両替に、50, 25, 10, 5, 1セント硬貨で何通りの表し方があるか。
; 硬貨をある順に並べたとき、
; n種類の硬貨で金額aを両替しようとすると
;
; * 最初の種類の硬貨意外を使う場合の数
;   +
; * n種類の硬貨を使う、金額 a-d (d: 最初の硬貨の額面金額) を両替する場合の数
;
; という和で表される。


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)
        )
  )

(print (count-change 100))



