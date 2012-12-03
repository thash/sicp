;; ある人がある部門で働くが, その部門で働く監督者を持たなければ, その人をその部門の黒幕(big shot)とする規則を定義せよ.

;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

(assert! (rule (same ?x ?x)))
(assert! (rule (big-shot ?person)
               (and (job ?person (?section . ?type))
                    (supervisor ?person ?supervisor)
                    (job ?supervisor (?section-sp . ?type-sp))
                    (not (same ?section ?section-sp)))))

;; big shot of computer section
;;; Query input:
(and (job ?who (computer . ?type))
     (big-shot ?who))
;;; Query results:
(and (job (Bitdiddle Ben) (computer wizard)) (big-shot (Bitdiddle Ben)))

;; big shot of accounting section
;;; Query input:
(and (job ?who (accounting . ?type))
     (big-shot ?who))
;;; Query results:
(and (job (Scrooge Eben) (accounting chief accountant)) (big-shot (Scrooge Eben)))

