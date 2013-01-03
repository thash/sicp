; ヒャッハー http://d.hatena.ne.jp/nTeTs/20090716/1247671614
;
; 二進モービル => 原著では binary mobile.
; http://www.google.co.jp/search?q=binary+mobile&um=1&ie=UTF-8&hl=ja&tbm=isch&source=og&sa=N&tab=wi&ei=0EhwT8OeMIOJmQWunNS3Bg&biw=1366&bih=670&sei=EklwT4EkocqYBYfspKUG
; インテリアとかであるやつ。 正しい日本語訳は「モービル」だそうで。
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

;; ひとつの枝は
;;   * length
;;   * structure = 単なる錘を表す数 or 別のモービル
;; からなる.
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

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

; rindaiの回答 - サブ手続きbranch-weightを使う
; どう考えてもhas-weight?よりmobile?のほうがよかった。
(define (total-weight2 mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (left-branch mobile))))

(define (branch-weight branch)
  (if (number? (branch-structure branch))
    (branch-structure branch)
    (total-weight2 (branch-structure branch))))

; で、さらにリファクタリング。
(define (total-weight3 mobile)
  (define (mobile? x) (pair? x))
  (if (mobile? mobile)
    (+ (total-weight3 #?=(left-branch mobile))
       (total-weight3 #?=(right-branch mobile)))
    mobile ; pairじゃなけりゃおもりなのでただの数字として返す。
    ))
; あれ、うまくいかない。大きぎる値が返る。



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

; この回答はよかろう。

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

