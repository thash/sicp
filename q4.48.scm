;; 文法を拡張せよ.
;; 名刺句や動詞句を拡張し, 形容詞や副詞を含めるように, あるいは合成文が使えるようにできる.

;; > この種の文法はいくらでも複雑になりうる. しかし実際の言語理解に関する限り, それらはままごとでしかない. 計算機による実際の自然言語理解は構文解析と意味解釈の絶妙な混合を必要とする.

;; 準備: sec4.3.2-Examples-of-Nondeterministic-Programs.scm を開き手続きをdriver-loop中でがりがり読み込む.

(define adjectives '(adjective nice tall diligent small white))

(define (parse-simple-noun-phrase)
  (define (parse-adjective-noun)
    (amb (parse-word nouns)
         (list 'adjective-noun
               (parse-word adjectives)
               (parse-adjective-noun))))
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-adjective-noun)))

;; test
(parse '(the tall professor lectures))
; => ;;; Amb-Eval value
;    (sentence (simple-noun-phrase (articles the) (adjective-noun (adjective tall) (noun professor))) (verbs lectures))

(parse '(the nice tall professor lectures))
; => ;;; Amb-Eval value
;    (sentence (simple-noun-phrase (articles the) (adjective-noun (adjective nice) (adjective-noun (adjective tall) (noun professor)))) (verbs lectures))



