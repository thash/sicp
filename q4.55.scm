;; データベースから, 次の情報を検索する単純質問を示せ.
;; (a). Ben Bitdiddleに監督されているひとすべて
;; (b). 経理部門(accounting division) のすべての人の名前と担当
;; (c). Slumervilleに住む人すべての名前と住所

(load "./sec4.4-Serendip")

;; 4.4.1. 推論的情報検索 サンプルデータ
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

;;; Query input:
(job ?x (computer programmer))
(job ?x (computer ?type))


;*** ERROR: invalid application: (#<promise 0x100740840>)
;Stack Trace:
;_______________________________________
;  0  (find-assertions query-pattern frame)
;        At line 76 of "./sec4.4-Serendip.scm"
;  1  (map stream-car argstreams)
;        At line 63 of "./sec3/stream-common.scm"
;  2  (stream-map proc s)
;        At line 335 of "./sec4.4-Serendip.scm"
;  3  (qeval q (singleton-stream '()))
;        At line 49 of "./sec4.4-Serendip.scm"
;  4  (stream-map (lambda (frame) (instantiate q frame (lambda (v f
;        At line 43 of "./sec4.4-Serendip.scm"
;  5  (display-stream (stream-map (lambda (frame) (instantiate q fr
;        At line 42 of "./sec4.4-Serendip.scm"

