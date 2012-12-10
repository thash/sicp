;; 創世記問題続編.
;; grandson(孫)のさらに一個下 great-grandson

(load "./sec4.4-Serendip")
;(query-driver-loop)

;; q4.63.scm からのcopy {{{1
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
;; }}}1

(assert! (rule ((great . ?rel) ?x ?y)
               (and (son-of ?x ?w) ;; ?xのsonを?wに束縛
                    (?rel ?w ?y)))) ;; ?wと(引数である)?yの間の関係を?relで取得
(assert! (rule ((grandson ) ?x ?y)
               (grandson-of ?x ?y)))


;; 使ってみる ;;
;;; Query input:
((great grandson) ?g ?ggs)
;;; Query results:
((great grandson) Mehujael Jubal)
((great grandson) Irad Lamech)
((great grandson) Mehujael Jabal)
((great grandson) Enoch Methushael)
((great grandson) Cain Mehujael)
((great grandson) Adam Irad)


;;; Query input:
(?relationship Adam Irad)
;;; Query results:
((great grandson) Adam Irad)
((great great . son) Adam Irad)
((great . grandson-of) Adam Irad)
((great great . son-of) Adam Irad)
;; 言い方が違うだけの結果が複数.


;; もっと長いの見つけてみる
;;; Query input:
((great great great grandson) ?g ?ggggs)
;;; Query results:
((great great great grandson) Enoch Jubal)
((great great great grandson) Cain Lamech)
((great great great grandson) Enoch Jabal)
((great great great grandson) Adam Methushael)
