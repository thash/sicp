(load "./sec2/q2.68.scm")
(use gauche.test)

(test-start "q2.68")
(test-section "symbol-included?")
(eqt '#t (symbol-included? 'A (symbols sample-tree)))
(eqt '#t (symbol-included? 'B (symbols sample-tree)))
(eqt '#t (symbol-included? 'C (symbols sample-tree)))
(eqt '#t (symbol-included? 'D (symbols sample-tree)))
(eqt '#f (symbol-included? 'Z (symbols sample-tree)))

(test-section "encode-symbol")
(eqt '(0)     (encode-symbol 'A sample-tree))
(eqt '(1 0)   (encode-symbol 'B sample-tree))
(eqt '(1 1 1) (encode-symbol 'C sample-tree))
(eqt '(1 1 0) (encode-symbol 'D sample-tree))

(test-section "encode")
(eqt '(0 1 1 0 0 1 0 1 0 1 1 1 0) (encode '(A D A B B C A) sample-tree))


