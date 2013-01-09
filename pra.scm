;;; [解読メモ] 5.2.1 計算機モデル

(define (make-machie register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
      ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; for-eachはリストの要素に手続きを順番に作用させる.
;; mapと違う点は返り値が作用させた結果リストではないところ.
(define (for-each proc lst)
  (if (null? lst)
    'done
    (begin
      (proc (car lst))
      (for-each proc (cdr lst)))))
;; 途中で処理した後の要素を溜めたりしてないことがわかる. procかけて, かけ捨て.

;; ここで言うとmachineに対する'allocate-registerは破壊的操作でmachineの中身自体が変容する. (そして最後にmachineを返している)
    (for-each (lambda (register-name)
        ((machine 'allocate-register) register-name)) ; procここまで
              register-names) ; lst

;; のこりのinstall-operationsもinstall-instruction-sequenceも同じでmachineに何かを設定している.
;; まとめるとmake-machieはOOPで言うコンストラクタのような役割か.


