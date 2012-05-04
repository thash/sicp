(load "./sec2.3.4")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; gosh> sample-tree
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) #0=(D C) 2) #1=(B . #0#) 4) (A . #1#) 8)
;
; gosh> (decode sample-message sample-tree)
; CALL decode-1 (0 1 1 0 0 1 0 1 0 1 ...) ((...) ((...) ...) (...) 8)
;   CALL decode-1 (1 1 0 0 1 0 1 0 1 1 ...) ((...) ((...) ...) (...) 8)
;     CALL decode-1 (1 0 0 1 0 1 0 1 1 1 ...) ((...) ((...) ...) (...) 4)
;       CALL decode-1 (0 0 1 0 1 0 1 1 1 0) ((leaf ...) (leaf ...) (...) 2)
;         CALL decode-1 (0 1 0 1 0 1 1 1 0) ((leaf ...) ((...) ...) (A ...) 8)
;         RETN decode-1 (A B B C A)
;       RETN decode-1 (D A B B C A)
;     RETN decode-1 (D A B B C A)
;   RETN decode-1 (D A B B C A)
; RETN decode-1 (A D A B B C A)
; (A D A B B C A)
