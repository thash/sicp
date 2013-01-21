;; 与えられた制御器で計算機を実装する時に必要なデータパスを決めるのを助けるため, シミュレータを使うことができる.
;; アセンブラを拡張し, 次の情報を計算機モデルに格納するようにせよ.
;;
;;   * 命令の型(assign, gotoなど)で, 格納されたすべての(異なる)命令のリスト
;;   * 入り口を保持するのに使った(異なる)レジスタのリスト(goto命令の参照するレジスタである)
;;   * save, restoreされる(異なる)レジスタのリスト
;;   * 各レジスタに対し, (異なる)代入元のリスト
;;       例: 図5.11の階乗計算機で, レジスタvalの代入元は(const 1)と((op * ) (reg n) (reg val))である
;;
;; 計算機のメッセージパッシングインターフェイスを拡張し, これらの新しい情報にアクセスできるようにせよ.
;; 解析プログラムをテストするため, 図5.12のFibonacci計算機を定義し, 構成したリストを調べよ.


;; 手順: 1. update-insts!実行時, make-execution-procedureを呼び出すとき同時にリストへ分類
;;       2. オリジナルのupdate-insts!のように返り値を捨てず, 分類リストを返すようにする.
;;       3. make-new-machineでmachineにinfoへの口を用意
;;       4. assembleは更新されたinstsと同時にinfoも返すようにする
;;       5. make-machineでinstall-instruction-sequenceをする時, assembleの返り値のcarを利用
;;       6. make-machineでinstall-infoを実行し, infoをsetする

;; 呼び出し階層: make-machine > assemble > extract-labels > update-insts!
;;                            > make-new-machine

(define (unique lst)
  (cond ((null? lst) lst)
        ((member (car lst) (cdr lst))
         (unique (cdr lst)))
        (else (cons (car lst)
                    (unique (cdr lst))))))

;; 1. update-insts!実行時, make-execution-procedureを呼び出すとき同時にリストへ分類
;; 2. オリジナルのupdate-insts!のように返り値を捨てず, 分類リストを返すようにする.
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))

        ;;; 追加 >>>
        (info-instructions '()) ;; すべての命令リスト
        (info-destinations '()) ;; goto命令の参照するレジスタ
        (info-stacked-regs '()) ;; save, restoreされるレジスタリスト
        (info-reg-sources '())) ;; 各レジスタに対する代入元リスト
        ;;; <<< 追加

    (for-each
      (lambda (inst)
        ;;; 追加 >>>
        ;; info-instructions: 命令はすべて格納
        (if (not (member (instruction-text inst) info-instructions))
          (set! info-instructions (cons (instruction-text inst)
                                        info-instructions)))

        ;; inst = ((goto (label test-b))) から test-bを取得
        (if (eq? (car (instruction-text inst)) 'goto)
          (set! info-destinations (cons (cadadr (instruction-text inst))
                                    info-destinations)))

        (if (or (eq? (car (instruction-text inst)) 'save)
                (eq? (car (instruction-text inst)) 'restore))
          (set! info-stacked-regs (cons (cadr (instruction-text inst))
                                    info-stacked-regs)))

        (if (eq? (car (instruction-text inst)) 'assign)
          ;; assignした時のregister nameをkeyとしてassocで検索
          (let ((record (assoc (cadr (car (instruction-text inst))))))
            (if record ;; 既に存在すればsourceを追加
              (set-cdr! record (cons (caddr (car (instruction-text inst))) ;; TODO
                                     (cdr record)))
              ;; 存在しない場合(register-name . (list...)) のような要素を新規作成
              (set! info-reg-sources (cons (cons (cadr (car (instruction-text inst)))
                                                 (list (caddr (car (instruction-text inst)))))
                                           info-reg-sources)))))
        ;;; <<< 追加

        ;; オリジナルのupdate-insts!処理
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops)))
      insts))

  ;; return info lists (追加)
  (list (unique info-instructions)
        (unique info-destinations)
        (unique info-stacked-regs)
        info-reg-sources))



;; 3. make-new-machineでmachineにinfoへの口を用意
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;;; 追加 >>>
        (machine-instructions '())
        (machine-destinations '())
        (machine-stacked-regs '())
        (machine-reg-sources  '()))
        ;;; <<< 追加

    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))

      ;;; 追加 >>>
      (define (install-info info-list)
        (set! machine-instructions (car    info-list))
        (set! machine-destinations (cadr   info-list))
        (set! machine-stacked-regs (caddr  info-list))
        (set! machine-reg-sources  (cadddr info-list)))
      ;;; <<< 追加

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)

              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;;; 追加 >>>
              ((eq? message 'install-info) install-info)
              ((eq? message 'get-instructions) machine-instructions)
              ((eq? message 'get-destinations) machine-destinations)
              ((eq? message 'get-stacked-regs) machine-stacked-regs)
              ((eq? message 'get-reg-sources)  machine-reg-sources)
              ;;; <<< 追加
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


;; 4. assembleは更新されたinstsと同時にinfoも返すようにする
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (let ((info (update-insts! insts labels machine))) ;; 変更
                      (cons insts info))))) ;; 変更


;; 5. make-machineでinstall-instruction-sequenceをする時, assembleの返り値のcarを利用
;; 6. make-machineでinstall-infoを実行し, infoをsetする
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))

    (for-each (lambda (register-name)
      ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ;;; 変更 >>>
    (let ((assemble-result (assemble controller-text machine)))
      ((machine 'install-instruction-sequence) (car assemble-result))
      ((machine 'install-info) (cdr assemble-result)))
    ;;; <<< 変更
    machine))
