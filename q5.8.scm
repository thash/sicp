;; 次のレジスタ計算機の命令はラベルhereが複数回定義してあるので曖昧である.
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (1). これまでに書いたシミュレータでは, 制御がthereに達した時レジスタaの内容はどうなるか.

;; 回答: 3になる.

;; label here を実行した時どこが参照されるか
;; lookup-labelが利用しているassocは最初に見つけた要素を返す
; gosh> (assoc 'b '((a 1) (b 2) (b 3)))
; (b 2)

;; label表にはassign 3するほうが先に格納されていると予測.

;; make-machine内install-instruction-sequenceでtextを操作する時,
;; extract-labelsですべてのlabelを走査する. start, here x 2, thereがsymbol?で真となり
;; make-label-entry結果がlabelsの頭にconsされる.
;; make-label-entryはlabel-nameと"その時点での"instsのcons.
;; instsには, textが"後ろから順に"格納されていく. extract-labels中でdisplayするとわかる

;; DEBUG: symbol? next-inst以下でlabelsに格納されている値をdisplayする.
;    +(begin
;     (receive insts
;              (cons (make-label-entry next-inst ; [new] make-label-entry
;                                      insts)
;                    labels))
;    +(display next-inst)
;    +(display " ::: ")
;    +(display insts)
;    +(display "\n----------\n")
;    +(display (cons (make-label-entry next-inst insts) labels))
;    +(display "\n==========\n")
;    +)
;; labelsにhereをkeyとしたinstsがふたつ登録され, assign 3する方が前方にある.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (2). extract-labels手続きを修正し, 同じラベル名が2つの異なる場所を指すように使われたら, エラーとなるようにせよ.
(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          ;; この部分にlabelsのチェックを追加
                          (let ((label (assoc next-inst labels)))
                            (if label
                              (error "Label already defined -- EXTRACT-LABELS" next-inst)
                              (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels))))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 動作テスト
(define x (make-machine '(a) (list (list '+ +)) '(start (goto (label here)) here (assign a (const 3)) (goto (label there)) here (assign a (const 4)) (goto (label there)) there)))

gosh> (start x)
done
gosh> (get-register-contents x 'a)
3

;; 修正版extract-labelsを使った時

gosh> !!!*** ERROR: Label already defined -- EXTRACT-LABELS here!!!

; ok
