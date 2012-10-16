;;  フレームをリストの対で表現する代わりに, 各束縛が名前-値の対であるような束縛のリストでフレームを表現せよ.
;; 「各フレームは変数を対応する値に対応づける束縛の表である」
;; という定義を満たせば, 実装方法はなんでもいいっちゃいい.
;;
;;     * 本文中のframe => ((a b c) . (1 2 3))
;;     * 問題4.11のframe => ((a . 1) (b . 2) (c . 3))

;; enclosing-environment, first-frame, the-empty-environmentは同じ

;; 基本要素となるframeの作り方がまず違う. zip的処理.

;; 回答: 初歩的なiterで回す
;(define (make-frame variables values)
;  (define (iter vars vals)
;    (if (or (null? vars) (null? vals))
;      '()
;      (cons
;        (cons (car vars) (car vals))
;        (iter (cdr vars) (cdr vals)))))
;  (iter variables values))
;
;(define (frame-variables frame)
;  (if (null? frame)
;    '()
;    (cons (caar frame) (frame-variables (cdr frame)))))
;
;(define (frame-values frame)
;  (if (null? frame)
;    '()
;    (cons (cdar frame) (frame-values (cdr frame)))))

;; つーかそんな面倒なことやらなくても,
;; 各処理のアタマにmapを加えるだけでよかったらしい.
(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

;; 最後, bindする
(define (add-binding-to-frame! var val frame)
  (append frame (cons (cons var val) '())))
;; 最後完全listにするため

