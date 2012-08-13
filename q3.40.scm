;; 次の計算を実行した結果として取り得るxをすべて列挙せよ。
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; P1: 参照, 参照, set.
;; P2: 参照, 参照, 参照, set.
;;
;; 逐次的に書き出すのは根気があればできるけど、一般的にできないもんかな。まいっか。

;; refP1 -> refP1 -> setP1 -> refP2 -> refP2 -> refP2 -> setP2
;;   10       10      100       100    100    100     1000000
;; refP1 -> refP1 -> refP2 -> refP2 -> refP2 -> setP2 -> setP1
;; ...
;;
;;  refP1 refP1       setP1
;;  refP2 refP2 refP2 setP2
;;  トランプ的な。出せる順が決まっている。

;; a.rb
;; $x = $x * $x
;; b.rb
;; $x = $x * $x * $x
;;
;;
;; 例示は理解の試金石ってか。


