;; disjoinとstream-flatmapが単に連結しないでストリームを差込みにするのはなぜか. 差込みがうまく働くのがわかる例を示せ. (ヒント: 3.5.3 interleave)

;; or に使われるdisjoin. 実装はこうなってる
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts)
                      frame-stream)))))

;; ストリームを差し込み, とは?
;; interleave で sec3/sec3.5.3.scm 再帰の最後で引数の順番を入れ替えて2個のstreamを順々に処理していくようになってる.
;; disjoinも同じと考えれば, 引数1個目が無限streamでもちゃんと動くように留意しているのでは.


