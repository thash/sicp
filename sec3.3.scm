;; 3.3. 可変データでのモデル化
;;    選択子と構成子に加え、データオブジェクトを変更するための変更子(mutator)を含んだデータ抽象を設計する
;;    変更子 - つまりsetter.
;;    変更子の定義されているオブジェクトを可変データオブジェクト(mutable data objectA)という
;;    本節では, 合成データ作成に使ったcons -- "対"に対してmutatorを定義し, 拡張する.

;; 3.3.1. 可変リスト構造
;;    set-car! とset-cdr! * set!と同じく返り値は重要ではない(実装依存).
;;    (set-car! x y) とすれば、xのcarが示す先をyで置き換える。 * これによってアクセスしなくなったデータ -- garbageが発生する
;;
;;    裏返しに(?), set-car!とset-cdr!を使ってconsを実装することも可能。
(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))


