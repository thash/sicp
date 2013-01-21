;; > make-machineの引数としてレジスタリストを要求するのではなく, 制御器の命令列を使って計算機の持つレジスタを決めるようにシミュレータを設計せよ.
;; > make-machineであらかじめレジスタを割り当てる代わりに, 命令のアセンブリ中に, 初めて見る度に, レジスタを割り当てることができる.

;; update-insts!実行中, レジスタを参照する命令が出てくるたびに新規かどうか判定して新規であればレジスタを割り当てる
;; レジスタを参照する命令は assign, reg, save, restore の4つ
;; このうちregのみ, レジスタ名が引数部分に出てくることに注意

(define (make-machine ops controller-text) ;; register-names を引数から削除
  (let ((machine (make-new-machine)))
    ;; assemble内で割り当てるので必要なくなる
    ;; (for-each (lambda (register-name)
    ;;   ((machine 'allocate-register) register-name))
    ;;           register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))


(define (unique lst)
  (cond ((null? lst) lst)
        ((member (car lst) (cdr lst))
         (unique (cdr lst)))
        (else (cons (car lst)
                    (unique (cdr lst))))))

;; 個々のinst文からregisterを見つける
(define (scan-register inst)
  ;; inst例: ((assign t (op rem) (reg a) (reg b)))
  (let ((result '()))
    ;; inst文がassign, save, restoreなら第一引数はまずレジスタ
    (if (or (eq? (caar inst) 'assign)
            (eq? (caar inst) 'save)
            (eq? (caar inst) 'restore))
      (set! result (cons (cadar inst) result)))
    ;; inst文を走査して(reg x)の形があればxをresultに加える
    (for-each
      (lambda (arg)
        (if (and (pair? arg)
                 (eq? (car arg) 'reg))
          (set! result (cons (cadr arg) result))))
      (cddr (car inst)))
    result))


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))
        (known-registers '())) ;; 追加

    (for-each
      (lambda (inst)
        ;; inst例: ((assign t (op rem) (reg a) (reg b)))
        ;;; 追加 >>>
        (for-each
          (lambda (reg)
            (if (not (member reg known-registers))
              (begin
                ((machine 'allocate-register) reg)
                ;; 二回目以降は初期化しないようにknown-registersへ保存
                (set! known-registers (cons reg known-registers)))))
          (unique (scan-register inst)))
        ;;; <<< 追加
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops)))
      insts)))
