;; qevalした結果がただひとつのstreamだったら成功. ソレ以外だったら失敗.
(load "./sec4.4-Serendip")

(define (uniquery-asserted pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((stream (qeval (negated-query pattern)
                           (singleton-stream frame))))
        (if (singleton-stream? stream)
          stream
          the-empty-stream)))
    frame-stream))
(put 'unique 'qeval uniquery-asserted)

;; s全体はnullじゃないけどstream-cdrするとnull.
;; => 最初の一個, stream-carの位置しか入ってないstream.
(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))
;; 本文中のsingleton-streamもそうだっけ?
;; あれは同じ要素でstreamが埋まってる話じゃなかったの?


;; uniqueの実装後にdriver-loopを稼働.
(query-driver-loop)

;;; 4.4.1. 推論的情報検索 サンプルデータ {{{1
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))
(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))
(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))
(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer) (computer programmer trainee)))
(assert! (can-do-job (administration secretary) (administration big wheel)))
;; }}}1

(unique (job ?x (computer programmer)))
;; だとダメ
(unique (job ?x (computer wizard)))
;; だと結果出る
;; => (unique (job (Bitdiddle Ben) (computer wizard)))

;; yet another test
(and (supervisor ?person ?boss) (unique (supervisor ?other ?boss)))
;; => (and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
;; => (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
