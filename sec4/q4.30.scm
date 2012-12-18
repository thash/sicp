;; Cy > eval-sequenceでもactual-value使えばよくね?
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env) ;; ここがevalからactual-valueになってる
              (eval-sequence (rest-exps exps) env))))

;; 簡略化すれば, http://sioramen.sub.jp/blog/2008/02/sicp-422.html
(define (f x)
  (g x)
  (h x))

;; これを(f 1)のように実行するとき
(f 1)
;; ↓
((g (thunk 1))
 (h (thunk 1)))
;; これは(h (thunk 1))の結果が返り値となり, gの方はforceされない...
;; つまり途中の式gが副作用(表示/代入)を持っていたとしても発動しないのではないか? というのがCyの懸念.

;; Ben > 脳みそ湧いてんのか? このfor-eachを見てみろよ
(define (for-each proc items)
  (if (null? items)
    'done
    (begin (proc (car items))
           (for-each proc (cdr items)))))

;; ほら (Cyの言うようにeval-sequenceをactual-value化しなくても) 動くだろ?
(for-each (lambda (x) (newline) (display x))
          '(57 321 88))

;; (a). Benが正しい理由 = これが動く理由を述べよ
;;      => newlineとdisplayがprimitive-procedureだから.
;;      (実際は自分で評価器のprimitive-procedure一覧に追加しなきゃならんので手前味噌感あるが, まあそういう議論だ)

;; (b). Cy < それprimitive-procedureだから動いてんだろダボが. じゃあこんな例はどうだよ
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;; p1で素にset!した時と, p2でthunkされる箇所でsetするときでは結果が違うのでは? という論旨. 実際にやってみる.

(p1 1)
;; => (1 2) -- 元の評価器
;; => (1 2) -- Cyの評価器(eval-sequenceをactual-value化したver)
(p2 1)
;; => 1     -- 元の評価器
;; => (1 2) -- Cyの評価器(eval-sequenceをactual-value化したver)

;; 元の評価器の場合結果が1になるのは, p2がこんな感じに展開されるからである.
(p2 1)
;; ↓ p2最後の評価式に適用(引数1は遅延される)
(p (set! x (cons x=(thunk 1) '(2))))
;; ↓ pを適用
((thunk (set! x (cons x=(thunk 1) '(2))))
 x=(thunk 1))


;; (c). Cy < ついでに言えば(a)の例でも俺のeval-sequenceは動くよ.
;;      => まあdisplay, newlineがprimitive-procedureですから.

;; (d). 議論. 遅延評価においてsequenceはどう扱うべきか.
;;      本文の方法だと最後しかforceされないので直感に反し, 使いづらい.
;;      一方Cyの方法がわかりやすいが, せっかく遅延したのにsequenceを導入するだけでforceされてしまうのもうまくない. ぐぬぬ.


