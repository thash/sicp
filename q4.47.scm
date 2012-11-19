;; 出たLouis.
;; 動詞句は動詞 or 動詞+前置詞句だから, parse-verb-phrase を次のように定義するとはるかに直截であると言う.

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

;; これは動くか. さらにambの中の式の順序を交換しても動くか.

