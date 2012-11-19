;; 文を生成する
(define (parse-word word-list)
  (list (car word-list) (an-element-of (cdr word-list))))


; ;;; Amb-Eval input:
;
; (parse-sentence)
;
; ;;; Starting a new problem
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles the) (noun student)) (verbs studies))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles the) (noun student)) (verb-phrase (verbs studies) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles the) (noun student)) (verb-phrase (verb-phrase (verbs studies) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles the) (noun student)) (verb-phrase (verb-phrase (verb-phrase (verbs studies) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles the) (noun student)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verbs studies) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (articles the) (noun student)))))
