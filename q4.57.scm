;; rule の定義.
;; ref: http://www.serendip.ws/archives/2646

;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

;; 与えられた条件を論理プログラミング式に書き下すが, わかりにくい...
(assert! (rule (same ?x ?x)))
(assert! (rule (replace ?person-1 ?person-2)
               (and
                 (or (and (job ?person-1 ?job)
                          (job ?person-2 ?job))
                     (and (job ?person-1 ?job-1)
                          (job ?person-2 ?job-2)
                          (can-do-job ?job-2 ?job-1)))
                 (not (same ?person-1 ?person-2)))))

;; (a). Cy D. Fectに代わ(raplace)れる人すべて
;;; Query input:
(replace (Fect Cy D) ?who)
;;; Query results:
(replace (Fect Cy D) (Bitdiddle Ben))
(replace (Fect Cy D) (Hacker Alyssa P))

;; (b). 誰かに代われて, その誰かの方が多くの給料をもらっている人すべてと, 両者の給料
;;; Query input:
(and (salary ?person ?salary)
     (salary ?substitution ?salary-sub)
     (replace ?person ?substitution)
     (lisp-value < ?salary ?salary-sub))
;;; Query results:
(and (salary (Tweakit Lem E) 25000) (salary (Bitdiddle Ben) 60000) (replace (Tweakit Lem E) (Bitdiddle Ben)) (lisp-value < 25000 60000))
(and (salary (Fect Cy D) 35000) (salary (Hacker Alyssa P) 40000) (replace (Fect Cy D) (Hacker Alyssa P)) (lisp-value < 35000 40000))
(and (salary (Fect Cy D) 35000) (salary (Bitdiddle Ben) 60000) (replace (Fect Cy D) (Bitdiddle Ben)) (lisp-value < 35000 60000))
(and (salary (Hacker Alyssa P) 40000) (salary (Bitdiddle Ben) 60000) (replace (Hacker Alyssa P) (Bitdiddle Ben)) (lisp-value < 40000 60000))

;; Benを解雇しろ！

