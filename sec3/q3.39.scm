;; sec3.4.2.scm を次のように直列化する。

(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

;; これはsec3.4.2.scmの本文中で出てきた以下の例とは何が違うのか？
(s (lambda () (set! x (* x x))))

;; => serializeする範囲が違う。本設問中では2乗するところでのみ直列を保証している。
;;    ...ので、110がはじかれる。
;;    後半の(set! x (+ x 1))もserializeされているので11になる可能性もなくなり、
;;    結局
;;      101, 121, 100
;;    が残る。



