;; 本文の実装では, 定数やレジスタの他, labelにも演算を許している.
;;   [間違い] つまり (label (op +) (reg a) (reg b)) が可能になっている?
;;   [正解]   問題文は ((op +) (label hoge)) という命令文のことを言っていた.
;; これを修正し, 演算は定数とレジスタでのみ使えるようにせよ.

;; const, reg, labelが処理される場所, make-primitive-expを修正する?
;; => ではなく, その呼び出し元, make-operation-expのmap中にチェックを入れる

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                  ;; NEXT-LOOP: 厳密に問題文に従いかつ拡張されても動くように気遣うならば,
                  ;;            明示的にconsts/registerのみ許すという実装がベター
                 (if (label-exp? e) ;; *
                   (error "Can't make operation on label -- MAKE-OPERATION-EXP" e)) ;; *
                   (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
