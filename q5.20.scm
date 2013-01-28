;; 以下の式で作られたリスト構造の箱とポインタ表記及びfreeポインタは最初にp1であるとして,
;; (図5.14のような) メモリーベクタ表現を描け. freeの最後の値は何か. どのポインタがxとyの値を表しているか.
(define x (cons 1 2))
(define y (list x x))

;; 図
;; https://www.evernote.com/shard/s11/sh/ca819e25-3b36-4c0a-8085-3f33bab924ea/15f1ef4e69e5ccbc8b9d39a4a031860c
