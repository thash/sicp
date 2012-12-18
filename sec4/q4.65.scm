(load "./sec4.4-Serendip")
;(query-driver-loop)

;; wheelの定義は次の通り
(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))

;; wheelを出力すると重複がある.

;;; Query input:
(wheel ?who)

;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

;; wheel中身のandを直で打ってみると
;;; Query input:
(and (supervisor ?a ?b) (supervisor ?c ?a))

;;; Query results:
(and (supervisor (Scrooge Eben) (Warbucks Oliver)) (supervisor (Cratchet Robert) (Scrooge Eben)))
(and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (supervisor (Fect Cy D) (Bitdiddle Ben)))
(and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

;; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;; に対しandで検索をかけるが, Benが管理する人が3人いるから3個出てしまう
