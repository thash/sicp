;; データベースから, 次の情報を検索する単純質問を示せ.
;; (a). Ben Bitdiddleに監督されているひとすべて
;; (b). 経理部門(accounting division) のすべての人の名前と担当
;; (c). Slumervilleに住む人すべての名前と住所

;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

;; (a). Ben Bitdiddleに監督されているひとすべて
;;; Query input:
(supervisor ?x (Bitdiddle Ben))
;;; Query results:
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

;; (b). 経理部門(accounting division) のすべての人の名前と担当
;;; Query input:
(job ?x (accounting . ?type))
;;; Query results:
(job (Cratchet Robert) (accounting scrivener))
(job (Scrooge Eben) (accounting chief accountant))

;; (c). Slumervilleに住む人すべての名前と住所
;;; Query input:
(address ?x (Slumerville . ?type))
;;; Query results:
(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
