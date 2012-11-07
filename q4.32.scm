;; 遅延度が高いと何が嬉しいの
;; ref: http://sioramen.sub.jp/blog/2008/02/sicp-423.html


;; (1). 未知数ストリームが作れる.
;; 3章ストリームだと作れない
(define xs (cons-stream x xs))

;; けど, 遅延度が高いと未知数だらけの遅延リスト(= ストリーム)が作れる.
;; xて何やねん状態でもおｋ
(define cs (cons x xs))


;; (2). 無限ネストのストリームが作れる(mjd)
;; 3章ストリームだと作れないけど遅延リストならおｋ
(define endless (cons-stream endless '(1))) ;; ダメ
(define endless (cons endless '(1))) ;; OK

;; これ本質的には未定義変数を要素に含められることとちゃう？別個のメリットでないような.

