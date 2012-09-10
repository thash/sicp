;; 対が"ある有用な順"で現れるようなストリームを定義せよ。

;; weightは"重み関数" W(i,j)
(define (merge-weighted s1 s2 weight)
  )

;; これを使って、pairsをweightに基づいてmergeしよう。
(define (weighted-pairs s t weight)
  )

;; 陥った問題: 最初の項をweight関係なしにconsしてしまうと不十分。
;;             cons-streamを呼ばないとLouisの二の舞になるので、
;;             最初にdummyのpairを入れておいてcons-stream, とやるとよさげ。

;; (a).

;; (b).

