(load "./my_defs")
(prepare-test)

;; list要素がuniqueかどうかを判定するdistinct? procedure
(eqr (distinct? '()) => '#t)
(eqr (distinct? '(a)) => '#t)
(eqr (distinct? '(a b c d)) => '#t)
(eqr (distinct? '(a b c c)) => '#f)
(eqr (distinct? '(a b c a)) => '#f)
(eqr (distinct? '(a a a a)) => '#f)


