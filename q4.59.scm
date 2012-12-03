;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

;; (a). 金曜日のすべての会合を探す
;;; Query input:
(meeting ?section (Friday ?time))
;;; Query results:
(meeting administration (Friday 1pm))

;; (b). Alyssaの規則
;; > 自分の名前を指定し, 自分の会合を聞けたほうが有用と考えた.
;; > そこで彼女はある人の会合はwhole-companyの会合すべてと, その人の部門の会合のすべてを含むとする規則を設計した
(assert! (rule (meeting-time ?person ?day-and-time)
               (or (meeting whole-company ?day-and-time)
                   (and (job ?person (?section . ?type))
                        (meeting ?section ?day-and-time)))))


;;; Query input:
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
;;; Query results:
(meeting-time (Hacker Alyssa P) (Wednesday 4pm))
(meeting-time (Hacker Alyssa P) (Wednesday 3pm))

