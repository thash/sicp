;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.3.2. 非決定性プログラムの例
(load "./sec4.3-nondeterministic")

;(driver-loop)
;;; driver-loopを起動した後に定義する. >>> ココカラ
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; amb使えば論理パズル解けるよー (ええからはよ実装せえや)
;; 問題を論理的に書き下せる時点で解けとるようなもんやん
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; multiple-dwellingを動かすにはdistinct?の実装が必要
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;;;; Amb-Eval input:
;(multiple-dwelling)
;;;; Starting a new problem
;;;; Amb-Eval value
;((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))


;; => q4.38.scm, q4.39.scm, q4.40.scm, q4.41.scm, q4.42.scm, q4.43.scm, q4.44.scm


;;; 自然言語の構文解析 ;;;
;; (ええからはよ実装せえや)

;; 品詞に分解
(define nouns '(noun student professor cat class))
(define verbs '(verbs studies lectures eats sleeps))
(define articles '(articles the a))

;; 文法.
;; The cat eats を構文解析すると
(sentence (noun-phrase (article the) (noun cat))
          (verb eats))

;; 道具箱
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


;; こうやって使う
(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;; 探索とバックトラックは複雑な文法を扱うとき本当に有効である(ええからはよ実装せえや)
(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

;; ようやくamb出てきた
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

;; ついでに名刺句の定義を改善する
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

;; ここまでやると, 以下のような複雑な文を解析できる(うれしげに言うとらんと実装せえっちゅうに)
(parse '(the student with the cat sleeps in the class))
;; =>
(sentence
  (noun-phrase
    (simple-noun-phrase (articles the) (noun student))
    (prep-phrase (prep with)
                 (simple-noun-phrase
                   (article the) (noun cat))))
  (verb-phrase
    (verb sleeps)
    (prep-phrase (prep in)
                 (simple-noun-phrase
                   (article the) (noun class)))))

;; 別の例
(parse '(the professor lectures to the student with the cat))
;; =>
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase (prep to)
                   (simple-noun-phrase
                     (article the) (noun student))))
    (prep-phrase (prep with)
                 (simple-noun-phrase
                   (article the) (noun cat)))))
;; => 再実行
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase (prep to)
                 (noun-phrase
                   (simple-noun-phrase
                     (article the) (noun student))
                   (prep-phrase (prep with)
                                (simple-noun-phrase
                                  (article the) (noun cat)))))))

;; => q4.45.scm, q4.46.scm, q4.47.scm, q4.48.scm, q4.49.scm


