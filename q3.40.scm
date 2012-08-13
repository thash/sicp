;; 次の計算を実行した結果として取り得るxをすべて列挙せよ。
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; P1: 参照, 参照, set.
;; P2: 参照, 参照, 参照, set.
;;
;; 逐次的に書き出すのは根気があればできるけど、一般的にできないもんかな。


