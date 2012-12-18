;; Cainの孫Lamechの息子たち, Methushaelの孫たちを見つけるために
;;   "SがFの息子であり, かつFがGの息子であるなら, SはGの孫(grandson)である"
;;   "WがMの妻であり, かつSがWの息子であるなら, SはMの息子である"
;; のような規則を形式化せよ.

;;; answer ;;;
(load "./sec4.4-Serendip")
;(query-driver-loop)

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (son-of ?m ?s)
               (or (son ?m ?s)
                   (and (wife ?m ?w)
                        (son ?w ?s)))))
(assert! (rule (grandson-of ?g ?s)
               (and (son-of ?f ?s)
                    (son-of ?g ?f))))

;;; Query input:
(grandson-of Cain ?who)
;;; Query results:
(grandson-of Cain Irad)

;;; Query input:
(son-of Lamech ?who)
;;; Query results:
(son-of Lamech Jubal)
(son-of Lamech Jabal)

;;; Query input:
(grandson-of Methushael ?who)
;;; Query results:
(grandson-of Methushael Jubal)
(grandson-of Methushael Jabal)

;; 結果が重複してるな...

;; その他遊ぶ.
;;; Query input:
(grandson-of ?g ?p)

;;; Query results:
(grandson-of Methushael Jubal)
(grandson-of Methushael Jabal)
(grandson-of Mehujael Lamech)
(grandson-of Irad Methushael)
(grandson-of Enoch Mehujael)
(grandson-of Cain Irad)
(grandson-of Adam Enoch)
