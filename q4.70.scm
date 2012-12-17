;; 手続き add-assertion! と add-rule! のlet束縛の目的は何か.
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
       (set! THE-ASSERTIONS
             (cons-stream assertion old-assertions))
       'ok))

;; THE-ASSERTIONSの定義はこう. カラのstream.
(define THE-ASSERTIONS the-empty-stream)
;; でもdriver-loop起動してからassert!して, その状態でassertion一覧どなして見るの？

;; http://sioramen.sub.jp/blog/2009/12/sicp-4445-ex-470.html
;; メモリ番地で説明. わかりやすい

;; Scheme で cons-stream と set! の組み合わせが上の図のように変数を格納するのは、単に仕様である。
;;
;; しかし、一見なんの意味もない、省略可能な中間変数にしか見えない old-assertions が実は必須だった。というのは罠以外の何者でもない。
;;
;; この問題の意図は、cons-stream 使ったグローバル変数の set! には、こういう落とし穴があるので注意しましょう。ということである。


;; add-assertion! の次の実装(letを使わないversion)は何が悪いか.
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
    (cons-stream assertion THE-ASSERTIONS))
  'ok)



