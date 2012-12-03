;; 合成質問(compound queries) -- and, or, notを使う.
;; (a). Ben Bitdiddleが監督している人すべての名前とその住所
;; (b). 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料.
;; (c). 計算機部門に居ない人が監督している人すべてと, その監督者の名前と担当.

;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

;; (a). Ben Bitdiddleが監督している人すべての名前とその住所
;;; Query input:
(and (supervisor ?person (Bitdiddle Ben)) ;; ?x -> ?personにすると絞り込みが... どういう仕組か本文実装の時に見る.
     (address ?person ?where))
;;; Query results:
(and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(and (supervisor (Fect Cy D) (Bitdiddle Ben)) (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))


;; (b). 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料.
;;; Query input:
(and (salary ?person ?amount)
     (salary (Bitdiddle Ben) ?bens-amount) ;; 変数束縛のようなことをしてる...
     (lisp-value > ?bens-amount ?amount))
;;; Query results:
(and (salary (Aull DeWitt) 25000) (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 25000))
(and (salary (Cratchet Robert) 18000) (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 18000))
(and (salary (Reasoner Louis) 30000) (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 30000))
(and (salary (Tweakit Lem E) 25000) (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 25000))
(and (salary (Fect Cy D) 35000) (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 35000))
(and (salary (Hacker Alyssa P) 40000) (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 40000))

;; (c). 計算機部門に居ない人が監督している人すべてと, その監督者の名前と担当.
;;; Query input:
(and (job ?person ?section)
     (and (not (job ?person (computer . ?type)))
          (supervisor ?person ?supervisor)))
;;; Query results:
(and (job (Aull DeWitt) (administration secretary)) (and (not (job (Aull DeWitt) (computer . ?type))) (supervisor (Aull DeWitt) (Warbucks Oliver))))
(and (job (Cratchet Robert) (accounting scrivener)) (and (not (job (Cratchet Robert) (computer . ?type))) (supervisor (Cratchet Robert) (Scrooge Eben))))
(and (job (Scrooge Eben) (accounting chief accountant)) (and (not (job (Scrooge Eben) (computer . ?type))) (supervisor (Scrooge Eben) (Warbucks Oliver))))
