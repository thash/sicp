;; ルイズたんが平方器として考えた次の定義には
;; 重大な欠点があるのだがそれは何か、という問題。
(define (louis-squarer a b)
  (multiplier a a b))

;; これ、aを渡したときはいいけど、
;; bを渡すとmultiplierの中でprocess-new-valueが呼ばれた時m1 m2 productのproductのみがhas-valueである状態に陥り、aの値が出てこない。
;;  => だめじゃね。

