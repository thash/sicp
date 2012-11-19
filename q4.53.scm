;; 問題4.51 (q4.51.scm) に述べたようなpermanent-set!と, 問題4.52 (q4.52.scm) のようなif-failを使うと
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))
;; の評価の結果はどうなるか.

