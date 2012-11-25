;; (1). 分岐を左から右ではなくランダムな順に探す ramb を実装せよ.
(use srfi-27)
(define (random x) (random-integer x))
;; (random 3) で 0,1,2 のいずれかが返る

;; itemsからランダムに一つ取ってくる手続き
;; * ref: http://d.hatena.ne.jp/rui314/20070118/p1
;; * ref: http://practical-scheme.net/gauche/man/gauche-refj_44.html
(define (pick1 items)
  (if (null? items) #f
    (list-ref items (random (length items)))))

;; さらに, リスト中からpick1で選んだ1個を取り除くreject手続きを用意する.
(define (reject x xs)
  (cond ((null? xs) '())
        ((equal? x (car xs)) (cdr xs))
        (else (cons (car xs) (reject x (cdr xs))))))

;; 動作確認 {{{2
; gosh> (define x '(1 2 3 4 5 6 7 8 9))
; gosh> (pick1 x)
; 9
; gosh> (reject 9 x)
; (1 2 3 4 5 6 7 8)
; gosh> (pick1 '(1 2 3 4 5 6 7 8))
; 2
; gosh> (reject 2 '(1 2 3 4 5 6 7 8))
; (1 3 4 5 6 7 8) }}}2

;; analyze, amb?, amb-choises, analyze-ambをrambで置き換える.
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          (let ((item (pick1 choices)))
            (item env
                  succeed
                  (lambda ()
                    (try-next (reject item choices)))))))
      (try-next cprocs))))


;; (2). これが問題4.49 (q4.49.scm) のAlyssaの問題を救うことを示せ.
;; 脚注54: 文法は多くの場所で高度に再帰的であり, Alyssaの文章生成法は再帰の一つに「落ち込み】止まってしまう.

(load "./sec4.3-nondeterministic")
;; rambバージョンを試す場合はdriver-loopの前に上記の実装をload
(driver-loop)
;; (driver-loop)後, requireの実装と構文解析の材料をloadする {{{1
;; extracted with $ egrep "^[^;]" sec4.3.2-Examples-of-Nondeterministic-Programs.scm
(define (require p)
  (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define nouns '(noun student professor cat class))
(define verbs '(verbs studies lectures eats sleeps))
(define articles '(articles the a))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))
(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define prepositions '(prep for to in by with))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
;;; }}}1
;; なおここで, ランダムにwordを選ぶようにするため, an-element-ofの実装を ramb バージョンにする.
(define (an-element-of items)
  (require (not (null? items)))
  (ramb (car items) (an-element-of (cdr items)))) ;; amb -> ramb

;; q4.49.scm と同じ parse-word を定義
(define (parse-word word-list)
  (list (car word-list) (an-element-of (cdr word-list))))

;; 実行結果 {{{2
; ;;; Amb-Eval input:
; (parse-sentence)
;
; ;;; Starting a new problem
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles a) (noun cat)) (verbs eats))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles a) (noun cat)) (verb-phrase (verbs eats) (prep-phrase (prep to) (simple-noun-phrase (articles the) (noun professor)))))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles a) (noun cat)) (verb-phrase (verb-phrase (verbs eats) (prep-phrase (prep to) (simple-noun-phrase (articles the) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (articles a) (noun class)))))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles a) (noun cat)) (verb-phrase (verb-phrase (verb-phrase (verbs eats) (prep-phrase (prep to) (simple-noun-phrase (articles the) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (articles a) (noun class)))) (prep-phrase (prep for) (simple-noun-phrase (articles a) (noun cat)))))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; Amb-Eval value
; (sentence (simple-noun-phrase (articles a) (noun cat)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verbs eats) (prep-phrase (prep to) (simple-noun-phrase (articles the) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (articles a) (noun class)))) (prep-phrase (prep for) (simple-noun-phrase (articles a) (noun cat)))) (prep-phrase (prep for) (simple-noun-phrase (articles a) (noun student)))))
