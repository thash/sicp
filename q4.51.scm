;; 失敗の時にもやり直さない permanent-set! という新しい種類の代入を実装せよ.
;; 例えば次のように, リストから2つの異なる要素を選び, 成功した選択ができるまでに必要な試行数が得られる.
(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;; ここに実装


;; ここでpermanent-set!の代わりにset!を使ったらどうなるか.

