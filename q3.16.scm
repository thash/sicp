;; リストの構造中にある対の個数を数える。再帰的にやる。

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;; で、↑は間違っている。証拠として、3つの対でできていながら
;; 4,7,そして何も返さない構造を作る。

;; set-*!の破壊的作用を使ってやる。

(define l1 (list 'a))
(define l2 (list 'b 'c))
(define l3 (list 'a 'b 'c))

(set-car! l2 l1)
(set-car! (cdr l2) l1)

;; 図 => http://www.evernote.com/shard/s11/sh/694d35e2-9255-4e13-8b0b-316c97c46ea2/e1a77bf5462023acef235ec7d3f58e64
;; gosh> (count-pairs l2)
;; 4

(define x1 (list 'a))
(define x2 (list 'b))
(define x3 (list 'c))

(set-car! x1 x2)
(set-cdr! x1 x2)
(set-car! x2 x3)
(set-cdr! x2 x3)

;; 図 => http://www.evernote.com/shard/s11/sh/9816ae02-ee16-4ba2-baf4-254f098db21a/0e54218de91380dd0500c967a4ac99a3
;; gosh> (count-pairs x1)
;; 7

(define x-inf (list 'a 'b 'c))
(set-cdr! (cdr (cdr x-inf)) x-inf)

;; 図 => http://www.evernote.com/shard/s11/sh/9e1978a1-413d-4a15-a863-dcc52c928e6b/8c08668a8eeb969db8b61fd78e5c2871
;; gosh> (count-pairs x1)
;;  => 無限ループでフリーズ. 何も返さないってこれでいいの？

