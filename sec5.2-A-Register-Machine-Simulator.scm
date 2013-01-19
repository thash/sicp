;;;   5.2 レジスタ計算機シミュレータ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; controllerの演算をシミュレートしてくれるプログラムを作成する. こんなイメージ.
;    (make-machine <register-names> <operations> <controller>)
;    (set-register-contents! <machine-model> <register-name> <value>)
;    (get-register-contents <machine-model> <register-names>)
;    (start <machine-model>)

;; => q5.7.scm -- このシミュレータを使い, q5.4.scm で設計した計算機をテストせよ

;;;     5.2.1 計算機モデル machine-model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; machine
;; make-machineでは以下のことが行われる(Machine.new?)
;;   1. make-new-machine で裸のmachineを作る
;;   2. allocate-registerによって第一引数のregister-namesをmachineのregister-tableに登録
;;   3. install-operationsによって第二引数のopsをmachineのthe-opsに登録
;;   4. install-instruction-sequenceで第三引数の計算器文書本体をassemble(本体はextract-labels)走査, label一覧を保持し, 同時に命令列もmachine内に持つ.
;;   5. 以上の処理を行った実行可能なmachineを返す

;; machineはdispatchでコマンド渡してOOP的に動かす3章のアレ.
;; 未知: make-new-machine, assemble
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine))) ; basicなmachineを作成
    ;; for-eachはリストの要素に手続きを順番に作用させる(mapに対するeach).
    (for-each (lambda (register-name)
      ((machine 'allocate-register) register-name)) ; ここまでproc
              register-names) ; procに渡すlist
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) ; make-new-machineで作ったmachineにはtextが入ってない
     (assemble controller-text machine))
    machine))

;;; register
;; contentsに対するget/setを内包した変数
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))


;;; stack
;; 局所状態を持つ手続きとして表現できる.
(define (make-stack)
  ;; 捜査対象はまず空リスト
  (let ((s '()))
    ;; push: リストの先頭にくっつける
    (define (push x)
      (set! s (cons x s)))
    ;; pop: リストはcdrに更新しつつ, carを返す
    (define (pop)
      (if (null? s)
        (error "Enpty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
    ;; リストを初期化する.
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))


;;; 基本計算機
;; make-new-machine 手続きは, machine オブジェクトを作成する.
;; machine は局所状態としてregister table (初期registerはflagとpc(= program counter))を持つ.
;; machine は局所状態として演算リスト, 空の命令列なども持つ.
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    ;; the-opsの定義にstackを, register-tableの定義にpc,flagを使うため二重let.
    ;; Gaucheにはlet*というのがあるが.
    (let
      ;; 空の演算リスト. デフォルトでmachineの持つstackを初期化する演算のみ定義
      ;; [[op1, lambda1], [op2, lambda2]...]
      ((the-ops
         (list (list 'initialize-stack
                     (lambda () (stack 'initialize)))))
       ;; pc, flagのみ入ったregister table.
       ;; [[regname1, register1], [regname2, register2], ...]
       (register-table
         (list (list 'pc pc) (list 'flag flag))))

      ;; 指定された名前でregisterを新規作成, machineのregister tableに登録
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name) ; 既にその名のregisterがあれば
          (set! register-table ; keyとしてのnameとregister本体のlistを先頭にくっつける
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)

      (define (lookup-register name)
        (let ((val (assoc name register-table))) ; table形: (('regname register) ...)
          (if val ; val は#fもしくは('regname register)というリストが入っている
            (cadr val) ; registerを返す
            (error "Unknown register:" name))))

      (define (execute)
        ;; get-contentsはregisterに'get messageを送り, registerの値を得る
        ;; cond eq? message のstart内で, execute直前にpcへthe-instruction-sequenceがsetされている
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ;; instruction-execution-procはただのcdr.
              ;; pcレジスタに書くのされるデータ構造がわからないと何とも言えない
              ((instruction-execution-proc (car insts)))
              (execute))))) ;; instsがnullになるまで再帰.

      ;; register操作, stack取り出しなどの実行を指示される.
      (define (dispatch message)
        ;; startより先に2行下のinstall-instruction-sequenceを実行.
        ;; さもないとpcに何も入らなさそう
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ;; 引数をthe-instruction-sequence, 後のpc初期値へsetする
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ;; the-opsにはデフォルトのinitializeだけ入ってるので, 初期化だけどappendで
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack) ; 局所変数stackを取り出す
              ((eq? message 'operations) the-ops) ; 局所変数the-opsを取り出す
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(define (start machine)
  (machine 'start))
;; machineから一足飛びにregisterの中身を取り出すショートカット
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
;; 同, set版
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


;;;     5.2.2 アセンブラ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; > 命令実行手続きを生成する前に, アセンブラはすべてのラベルが何を参照するか知らなければならない.
;; > そこでラベルを実行手続きから分離するため, 制御器の文書の走査から始める.
;; > 文書を走査しながら, 命令のリストと, 各ラベルをリストの中へのポインタと対応づける表を構成する.

;; assemble: machineモデルに格納すべき命令列をcontroller-textから抽出して返す.
;; make-machine内で最後の仕上げに使われている.
(define (assemble controller-text machine)
  (extract-labels controller-text ; [new] extract-labels
                  (lambda (insts labels)
                    (update-insts! insts labels machine) ; [new] update-insts!
                    insts)))

;; 渡されたcontroller-textから最初の命令リストとラベル表を構築する
;; "textの要素を順に走査し, instsとlabelsを蓄積する."
(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text) ; textを減らしながら再帰
                    ;; receiveにはtextを処理する手続きが渡される.
                    ;; 引数insts: それぞれがtextの命令を含んでいる命令のデータ構造のリスト
                    ;; 引数labels: textの各ラベルを, リストinsts内のラベルが指示している位置と対応付ける表
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        ;; このへんでinstsの内容をdisplayしてみると動きがわかる.
                        (if (symbol? next-inst)
                          ;; "要素が記号(つまりラベル)の時は, labels表にlabel名とその時点でのinsts(継続?)を追加する."
                          (receive insts
                                   (cons (make-label-entry next-inst ; [new] make-label-entry
                                                           insts)
                                         labels))
                          ;; "それ以外の要素はinstsリストに追加する."
                          (receive (cons (make-instruction next-inst) ; [new] make-instruction
                                         insts)
                                   labels)))))))

;; instsに実行手続きを入れ更新して返す.
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc! ; [new] -- instsのcdrにprocをset!する
          inst
          (make-execution-procedure ; [new]
            (instruction-text inst) labels machine ; [new] instruction-text
            pc flag stack ops)))
      insts)))

;; "機械命令データ構造" を作る. (命令文書 . 実行手続き) の対.
;; 実行手続きの部分は最初何もないが, update-insts!で挿入される.
;; ただ動かすだけなら命令文書を保持しておく必要はないが, わかりやすいので持ち回る
(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;; label表の要素は対
(define (make-label-entry label-name insts)
  (cons label-name insts))

;; 項目はlookup-labelを使って探す
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label -- ASSENBLE" label-name))))

;; 補足: assocの使い方
; gosh> (assoc 'b '((a 1) (b 2) (b 3)))
; (b 2) -- 同じkeyが複数あるときは最初のlistを返す

;; => q5.8.scm

;;;     5.2.3 命令の実行手続きの生成
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assembleの呼び出すextract-labelsは手続きを引数に取る.
;; その際渡す手続きはupdate-insts!を含み, update-insts!内では
;; make-execution-procedureの実行結果でinstを上書きしていく.

;; 4.1.7のanalyze同様, 命令の型に従って処理を振り分ける.
;; make-* は手続きを返す.
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign  inst machine labels ops pc)) ; [new] make-assign
        ((eq? (car inst) 'test)
         (make-test    inst machine labels ops flag pc)) ; [new] make-test
        ((eq? (car inst) 'branch)
         (make-branch  inst machine labels flag pc)) ; [new] make-branch
        ((eq? (car inst) 'goto)
         (make-goto    inst machine labels pc)) ; [new] make-goto
        ((eq? (car inst) 'save)
         (make-save    inst machine stack pc)) ; [new] make-save
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc)) ; [new] make-restore
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc)) ; [new] make-perform
        (else (error "Unknown instruction type -- ASSENBLE"
                     inst))))

;;; assign命令に対する実行手続き ;;;
(define (make-assign inst machine labels operations pc)
  ;; target: まず更新対象のregisterを取得
  ;; value-exp: 呼び出し時点でのinstの内容例 => (assign b (const 1))
  ;;            assign-value-expは単なるcddrなので (const 1) が格納される.
  (let ((target
          (get-register machine (assign-reg-name inst))) ; [new] assign-reg-name
        (value-exp (assign-value-exp inst))) ; [new] assign-value-exp
    (let ((value-proc
            (if (operation-exp? value-exp) ; [new] operation-exp?
              ;; (op xxx)という形であれば対応するoperation(登録されているはず)を引張り出し,
              ;; 実行可能なlambdaにしてvalue-procへ束縛
              (make-operation-exp ; [new]
                value-exp machine labels operations)
              ;; opでなければprimitive-exp(const, reg, label)と想定.
              (make-primitive-exp ; [new]
                (car value-exp) machine labels))))
      (lambda ()
        ;; target(register)にoperation内容lambda or primitive-exp(これもlambda)をset
        (set-contents! target (value-proc))
        (advance-pc pc))))) ; [new] advance-pc

;; このふたつは単なるcadr系のalias.
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

;; pcの内容をcdrで上書きすることで, pcを次へ進める.
;; branchとgotoを除くすべてのmake-*手続はadvance-pcで終了する.
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;;; test命令に対する実行手続き ;;;
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst))) ; [new] test-condition
    (if (operation-exp? condition)
      (let ((condition-proc
              (make-operation-exp
                condition machine labels operations)))
        (lambda ()
          ;; condition部分を実行した結果をflag(register)に格納
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      ;; test内容はoperationでないとエラー
      (error "Bad TEST instruction -- ASSENBLE" inst))))

;; (test ...) の...部分(condition)を取得
(define (test-condition test-instruction)
  (cdr test-instruction))

;;; branch命令に対する実行手続き ;;;
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst))) ; [new] branch-dest
    (if (label-exp? dest) ; [new] label-exp?
      (let ((insts
              ;; label-exp-labelはただのcadrなのでラベル名を返す.
              ;; lookup-labelはlabels tableから名前で引いたvalueを返す. valueはそこの継続.
              (lookup-label labels (label-exp-label dest)))) ; [new] label-exp-label
        (lambda ()
          ;; flag registerにはtestでoperationの結果が格納されている.
          ;; flag真偽に応じて, pc(次に実行するテキスト)へlabels格納valueのinstsをsetするか,
          ;; 普段通りadvanceさせるかを分岐させている.
          ;; これ先に判定させたほうが微妙に効率よくない？ 特にlabelが多い時.
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))
      ;; branch引数に(label ...)以外が来るとエラー
      (error "Bad BRANCH instruction -- ASSENBLE" inst))))

;; (branch (label gcd-done)) => (label gcd-done)
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;;; goto命令に対する実行手続き ;;;
;; 上記branchの内容から, gotoの実体はlabelのvalによるpcの上書きであると予測できる.
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst))) ; [new] goto-dest
    (cond ((label-exp? dest)
           (let ((insts
                   (lookup-label labels
                                 (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest) ; [new]
           (let ((reg
                   (get-register machine
                                 (register-exp-reg dest)))) ; [new] register-exp-reg
             (lambda ()
               (set-contents! pc (get-contents reg)))))
      (else (error "Bad GOTO instruction -- ASSENBLE"
                   inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))


;;; その他(save, restore, perform)の命令に対する実行手続き ;;;
;; save対象のregister値をstackへpushし, advance-pcする.
;; instには(save continue)などが入ってくる
(define (make-save inst machine stack pc)
  ;; stack-inst-reg-nameは単なるcadrで, (save continue) からcontinueという名前を得る.
  ;; save対象はregisterなので, get-registerでregister本体を取り出しregに束縛.
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst)))) ; [new]
    (lambda ()
      (push stack (get-contents reg)) ;; 実行時点でのregの内容をstackへpush
      (advance-pc pc))))

;; instには(restore continue)などが入ってくる
;; stackは順々にpush/popするだけでregisterの不整合(nへcontinueの内容をset等)は見ていないため,
;; saveとrestoreを対称的に使うように気をつける必要がある?
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack)) ; stackからpopした値でregisterを更新
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; performはprintなど返り値を必要としないoperationに対して使われる.
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst))) ; [new] perform-action
    (if (operation-exp? action)
      (let ((action-proc
              (make-operation-exp
                action machine labels operations)))
        (lambda ()
          ;; 実行してpcを進めるだけ
          (action-proc)
          (advance-pc pc)))
      (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))


;;; 部分式の実行手続き ;;;
;; const, reg, labelが基本手続きとして定義されている
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp); [new]
         ;; (constant 1)などの場合. 単に1を返すだけのlambdaを返す.
         (let ((c (constant-exp-value exp))) ; [new] constant-exp-value
           (lambda () c)))
        ((label-exp? exp)
         ;; (label gcd-done)などの場合.
         ;; labels tableからlabel名と対になっている該当instsを取得して返す.
         (let ((insts
                 (lookup-label labels
                               (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         ;; (reg n)などの場合.
         ;; machineに入っているregisterから対象を見つけ, contentsを返すlambdaを返す
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
    (else
      (error "Unknown expression type -- ASSENBLE" exp))))

;; tagged-list? は昔定義したのを持ってくる
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))


;; expは((op hoge) (reg a)...)という形(operation-exp?で#tになる形)で渡されてくる
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations)) ; [new] lookup-prim, operation-exp-op
        (aprocs
          ;; 引数リストに対しmake-primitive-expをmapした結果をapecsに束縛.
          (map (lambda (e)
            (make-primitive-exp e machine labels))
               (operation-exp-operands exp)))) ; [new] operation-exp-operands
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; expに入ってくるのは((op =) (reg b) (const 0)) や ((reg b)) など
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
;; operationのsymbolを取り出す (operation-exp-op '((op =) (reg b) (const 0))) => =
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
;; operationの引数リストを取り出す. 上の例だと ((reg b) (const 0))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation -- ASSEMBLE" symbol))))

;; => q5.9.scm, q5.10.scm, q5.11.scm, q5.12.scm, q5.13.scm


;;; 動作テスト ;;;
;    (define gcd-machine
;      (make-machine
;        '(a b t)
;        (list (list 'rem remainder) (list '= =))
;        '(test-b
;           (test (op =) (reg b) (const 0))
;           (branch (label gcd-done))
;           (assign t (op rem) (reg a) (reg b))
;           (assign a (reg b))
;           (assign b (reg t))
;           (goto (label test-b))
;        gcd-done)))

;    gosh> (set-register-contents! gcd-machine 'a 206)
;    gosh> (set-register-contents! gcd-machine 'b 40)
;    gosh> (start gcd-machine)
;    gosh> (get-register-contents gcd-machine 'a)
;    2
