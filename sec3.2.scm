;; 3.2. 評価の環境モデル
;;   代入を許したことにより変数は値が格納される「場所」となった。
;;   置き換えモデルに代わる新たなモデルにおいて、その「場所」は「環境」の中に確保される。
;;   環境はフレーム(frames)の並びである(?). frameは束縛(bindings)の表である。
;;   bindingsは変数名とその値を対応づける。
;;   各frameは外側の環境へのポインタを持つ。
;;   「変数の値とは、環境でその変数の束縛を含んでいる最初のフレームでの変数の束縛で与えられる値である」 <= ?
;;
;; 3.2.1. 評価の規則
;;   手続きは常にコードと環境へのポインタである。
(define (square x)
  (* x x))
;; はlambdaを使った以下の式のsyntax sugarに過ぎない。
(define square
  (lambda (x) (* x x)))


;; 3.2.2. 単純な手続きの作用
;  ; 図 -> evernote:///view/1246867/s11/6b110e34-2aa3-424e-95ec-2c40f9122801/6b110e34-2aa3-424e-95ec-2c40f9122801/
(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))



