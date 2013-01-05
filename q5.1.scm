;; 反復的アルゴリズムを用いたfactorial(階乗)の計算
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))
;; 比較のため再帰的プロセスを示す(まだレジスタ計算機では動かない). {{{
(define (factorial n)
  (if (= n 1)
    1
    (* (factorial (- n 1)) n))) ; }}}


;; 本文中GCDの例をなぞって必要な要素を考える
;;
;; 計算機が覚えておく必要のある変数
;;     n, porduct, counter
;;       ...nは違う? => いや, machine内部で変わらず外部からsetするだけでregisterにあたる.
;; 必要な基本演算
;;     (> counter n) -- test
;;     (* counter product)
;;     (+ counter 1)
;; サイクル内でのレジスタへの値格納
;;     product <- (* counter product)
;;     counter <- (+ counter 1)

;; assignで演算の結果格納ok. gcdのココとか.
  (assign t (op rem) (reg a) (reg b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 回答
;;
;; controller
;; https://www.evernote.com/shard/s11/sh/2771e673-a347-41db-b866-6fb7bc186219/a68f63f33ce3c696328e358bb1e96e7d
;;
;; data path
;; https://www.evernote.com/shard/s11/sh/74232726-9de7-43e3-831f-3d9f1cedcc3d/2495a826423ac22ba621d19aa8546dc2
