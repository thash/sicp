(load "./my_defs")
(load "./sec3.3.2")

(use gauche.test)

(define q (make-queue))
;; そもそもgosh(Scheme)の表現として、こうなってる。
;; gosh> (cons 'a '())
;; (a)

(insert-queue! q 'a)
(eqr q => '(#0=(a) . #0#))
(eqr (front-queue q) => 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(eqr q => '((a b . #0=(c)) . #0#))
(eqr (front-queue q) => 'a)
(delete-queue! q)
(eqr (front-queue q) => 'b)
(delete-queue! q)
(eqr (front-queue q) => 'c)
(delete-queue! q)
(eqr (front-queue q) => (test-error)) ;; FRONT called with an empty queue.

(eqr q => '(() c))
;; あれ? cが残ってるよ? => queueの表示問題については q3.21.scm を参照.
;; 直観的にはfront-ptrの指す先を表示した方がいいよねと。

