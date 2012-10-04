(load "./sec3.3.2")

(define q1 (make-queue))

(insert-queue! q1 'a)
;; ((a) a) <= text
;; (#0=(a) . #0#) <= gosh output
;; ((a) a)#<undef> <= (display q1)

(insert-queue! q1 'b)
;; ((a b) a)
;; ((a . #0=(b)) . #0#)
;; ((a b) b)#<undef> <= (display q1)

(delete-queue! q1)
;; ((b) b)
;; (#0=(b) . #0#)
;; ((b) b)#<undef> <= (display q1)

(delete-queue! q1)
;; (() b) <= text
;; (() b) <= gosh output
;; (() b)#<undef> <= (display q1)

;; "Lisp印字プログラムはqueueをどう扱えばいいか知らないだけである。" という。
;; sec3.3.2.scm で定義したqueueは可変長リストの
;;   * 頭へのpointer (front-ptr)
;;   * 末尾の要素 (rear-ptr)
;; であるので, front-ptrをとってやれば表示したいリストが見える。

(define (print-queue q) (front-ptr q))
;; => q3.21-test.scm

