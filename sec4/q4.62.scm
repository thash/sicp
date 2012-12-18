;;; answer ;;;
;; first, exec sec4.4-Logic-Programming.scm

(assert! (rule (last-pair (?x . ()) (?x))))
(assert! (rule (last-pair (?x . ?y) ?z)
               (last-pair ?y ?z)))

;;; Query input:
(last-pair (3) ?x)
;;; Query results:
(last-pair (3) (3))

;;; Query input:
(last-pair (1 2 3) ?x)
;;; Query results:
(last-pair (1 2 3) (3))

;;; Query input:
(last-pair (2 ?x) (3))
;;; Query results:
(last-pair (2 3) (3))

;;; Query input:
(last-pair ?x (3))
;; => 無限ループ
;; ^C!!!*** UNHANDLED-SIGNAL-ERROR: unhandled signal 2 (SIGINT)!!!
