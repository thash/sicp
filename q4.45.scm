;; 準備: sec4.3.2-Examples-of-Nondeterministic-Programs.scm を開き手続きをdriver-loop中でがりがり読み込む.
;; 次の文は5通りに解析できる. 違いを説明せよ
(parse '(the professor lectures to the student in the class with the cat))

;;; Starting a new problem
;;; Amb-Eval value (1)
(sentence (simple-noun-phrase (articles the) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verbs lectures) (prep-phrase (prep to) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (articles the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (articles the) (noun cat)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value (2)
(sentence (simple-noun-phrase (articles the) (noun professor)) (verb-phrase (verb-phrase (verbs lectures) (prep-phrase (prep to) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (articles the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (articles the) (noun cat)))))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value (3)
(sentence (simple-noun-phrase (articles the) (noun professor)) (verb-phrase (verb-phrase (verbs lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (articles the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (articles the) (noun class)))))) (prep-phrase (prep with) (simple-noun-phrase (articles the) (noun cat)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value (4)
(sentence (simple-noun-phrase (articles the) (noun professor)) (verb-phrase (verbs lectures) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (articles the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (articles the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (articles the) (noun cat)))))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value (5)
(sentence (simple-noun-phrase (articles the) (noun professor)) (verb-phrase (verbs lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (articles the) (noun student)) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (articles the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (articles the) (noun cat)))))))))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(parse '(the professor lectures to the student in the class with the cat))


