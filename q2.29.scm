; ヒャッハー
; 錘(おもり)

(define (make-mobile left right)
  (list left right))

; structureはおもり(数字) or 別のmobile.
(define (make-branch length structure)
  (list length structure))

; mobileを作る時は左右にbranchが必要である
; branchは、長さ(第一引数)とその先に繋がる「おもり or mobile」(第二引数)で定義される。

; branch for test
(define b (make-branch 1 5))
; sample mobile from wiki
(define mobile1
  (make-mobile (make-branch 2 4)
               (make-branch 3
                            (make-mobile (make-branch 2 1)
                                         (make-branch 4 2)))))

; (a). 選択子 left-branchとright-branchを書け。
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

; (b). 全重量を返す手続きを書け。

; サブ手続きhas-weight?を定義。
; branchにつながっているのはおもりかどうか。（ちがうならmobileがぶら下がってる）
(define (has-weight? branch)
  (if (not (pair? (cadr branch))) #t #f))

; 回答。
(define (total-weight mobile)
  (+
    (if (has-weight? #?=(left-branch mobile))
      (cadr (left-branch mobile))
      (total-weight (cadr (left-branch mobile))))
    (if (has-weight? #?=(right-branch mobile))
      (cadr (right-branch mobile))
      (total-weight (cadr (right-branch mobile))))))

; leftとrightで同じことをしているのでさらに抽象化できそう。

; 再帰部分で (total-weight (cadr (right-branch mobile)
; とcadrしているのは、lengthを飛び越えてmobileを得るため


; (c). モーメントを考えて釣り合うかどうかの判定手続きbalanced?を作る
; 例のWikiではtorqueだった。物理...
; トルク（英語：torque）は、ある固定された回転軸を中心にはたらく、回転軸のまわりの力のモーメントである。一般的には「ねじりの強さ」として表される。力矩、ねじりモーメントとも言う。 from wikipedia

; まずmomentを定義
(define (moment branch)
  (* (car branch)
     (if (has-weight? branch)
       (cadr branch)
       (total-weight (cadr branch)))))

(define (balanced? mobile)
  (= (moment (left-branch mobile))
     (moment (right-branch mobile))))

; balanced mobile
(define mobile2
  (make-mobile (make-branch 2 4)
               (make-branch 8 1)))

; (d). listではなくconsでbranchとmobileを定義するようにした。

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; mobile1はこんな感じになる
; ((2 . 4) 3 (2 . 1) 4 . 2)

; 影響が出るのは、cdrを取るところ。
;
; gosh> (cdr (list 1 2))
; (2)
; gosh> (cadr (list 1 2))
; 2
; gosh> (cdr (cons 1 2))
; 2
; gosh> (cadr (cons 1 2))
; !!!Stack Trace:!!!
;
; cadrにしていた部分をすべてcdrにすればok

