(load "./my_defs")
(load "./q2.69")

(use gauche.test)

(test-start "q2.69")
(test-section "successive-merge")
(eqr (successive-merge '((leaf A 1) (leaf B 2)))
     => '((leaf A 1) (leaf B 2) (A B) 3))
(eqr (successive-merge '((leaf A 1) (leaf B 2) (leaf C 4)))
     => '(((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7))

(eqr (successive-merge (make-leaf-set '((A 4) (B 2) (C 1))))
     => '(((leaf C 1) (leaf B 2) (C B) 3) (leaf A 4) (C B A) 7))

(eqr (successive-merge (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))
     => '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) #0=(D C) 2) #1=(B . #0#) 4) (A . #1#) 8))
 ;    => '(((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)))
 ;    ↑これでは通らない...

