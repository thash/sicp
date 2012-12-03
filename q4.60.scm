;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

;; 問題文のlives-nearでは対の重複を考慮していないため順序を変えたものが含まれ2倍出てくる.
;; しかも自分自身も出てくる.
(assert! (rule (same ?x ?x)))
(assert! (rule (lives-near ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                    (address ?person-2 (?town . ?rest-2))
                    (not (same ?person-1 ?person-2)))))

;;; Query input:
(lives-near ?person-1 ?person-2)

;;; Query results: 二重に出てる！＞＜
(lives-near (Aull DeWitt) (Reasoner Louis))
(lives-near (Aull DeWitt) (Bitdiddle Ben))
(lives-near (Reasoner Louis) (Aull DeWitt))
(lives-near (Reasoner Louis) (Bitdiddle Ben))
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))
(lives-near (Bitdiddle Ben) (Aull DeWitt))
(lives-near (Bitdiddle Ben) (Reasoner Louis))


;; > この問題を解決するために、各人に id を割り当てて、2つの名前の間に優先順位を付ける
;; ふむ.
(assert! (id (Warbucks Oliver) 0))
(assert! (id (Bitdiddle Ben) 1))
(assert! (id (Hacker Alyssa P) 2))
(assert! (id (Fect Cy D) 3))
(assert! (id (Tweakit Lem E) 4))
(assert! (id (Reasoner Louis) 5))
(assert! (id (Scrooge Eben) 6))
(assert! (id (Cratchet Robert) 7))
(assert! (id (Aull DeWitt) 7))

(assert! (rule (lives-near-uniq ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                    (address ?person-2 (?town . ?rest-2))
                    (id ?person-1 ?id-1)
                    (id ?person-2 ?id-2)
                    (lisp-value > ?id-1 ?id-2)
                    (not (same ?person-1 ?person-2)))))


;;; Query input:
(lives-near-uniq ?person-1 ?person-2)

;;; Query results:
(lives-near-uniq (Aull DeWitt) (Reasoner Louis))
(lives-near-uniq (Aull DeWitt) (Bitdiddle Ben))
(lives-near-uniq (Reasoner Louis) (Bitdiddle Ben))
(lives-near-uniq (Fect Cy D) (Hacker Alyssa P))
