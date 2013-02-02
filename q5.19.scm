;; Alyssa P. Hackerはシミュレータにブレークポイントbreakpoint機能が欲しい.

;; 与えられたラベルのあとn番目の命令の直前にブレークポイントを設定する手続き
;    (set-breakpoint <machine> <label> <n>)
;; シミュレータがbreakpointに達すると, labelとbreakpointの距離を印字し, 命令の実行を中止する.
;; そこでAlyssaはget-register-contents, set-register-contents!を使うことが出来る.
;; その後,
;    (proceed-machine <machine>)
;; で実行を続行できなければならない. またbreakpointを削除/前削除する手続きも実装せよ.
;    (cancel-breakpoint <machine> <label> <n>)
;    (cancel-all-breakpoints <machine>)

;; 青山さん < これないと困るでしょ
;    (print-all-breakpoints <machine>)

;; machineのdispatchに加える.
;; 内部アレとしてbreakpoints, を持つ.
;; breakpoints = ((labelname . distance) (labelname . distance)... )

(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; [re-define]
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; new "fields" for the machines
        (labels '()) ; ++
        (breakpoints '())) ; ++

    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
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
              ;; 追加 >>>
              ; (print breakpoints)
              (let ((possible (filter (lambda (breakpoint)
                                        (display (list "bp-abs=" (breakpoint-absolute breakpoint)))
                                        (print ", linenum=" (instruction-line-count (car insts)))

                                        (= (breakpoint-absolute breakpoint)
                                           (instruction-line-count (car insts))))
                                      breakpoints)))
                ;; if not, continue
                (if (null? possible)
                  (execute-do)
                  ;; otherwise, print out breakpoint info and
                  ;; stop execution -- contents of pc remain
                  (begin
                    (display "Breakpoint hit -- ")
                    (for-each
                      (lambda (breakpoint)
                        (display (breakpoint-label breakpoint))
                        (display " ")
                        (display (breakpoint-offset breakpoint))
                        (display " "))
                      possible)
                    (newline))))))))

      ;; 旧execute本体をexecute-doへ 追加 >>>
      (define (execute-do)
        (let ((insts (get-contents pc)))
          ((instruction-execution-proc (car insts)))
          (execute)))
      ;; <<< 追加

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

              ;; 追加 >>>
              ((eq? message 'set-breakpoint!)
               (lambda (label offset)
                 (set! breakpoints (cons (make-breakpoint label offset labels)
                                         breakpoints))))
              ((eq? message 'cancel-breakpoint!)
               (lambda (label offset)
                 (set! breakpoints
                   (filter (lambda (breakpoint)
                             (not (and (eq? (breakpoint-label breakpoint)
                                            label)
                                       (eq? (breakpoint-offset breakpoint)
                                            offset))))
                           breakpoints))))
              ((eq? message 'cancel-all-breakpoints!)
               (lambda () (set! breakpoints '())))
              ((eq? message 'print-all-breakpoints)
               (lambda () (display breakpoints))) ; original
              ((eq? message 'proceed)
               (lambda () (execute-do)))
              ((eq? message 'set-labels)
               (lambda (lab) (set! labels lab)))
              ;; <<< 追加
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


;; external interfaces for breakpoints
(define (proceed-machine machine) ((machine 'proceed)))
(define (set-breakpoint! machine label offset)
  ((machine 'set-breakpoint!) label offset))
(define (cancel-breakpoint! machine label offset)
  ((machine 'cancel-breakpoint!) label offset))
(define (cancel-all-breakpoints! machine)
  ((machine 'cancel-all-breakpoints!)))
(define (print-all-breakpoints machine)
  ((machine 'print-all-breakpoints)))


;; selectors for breakpoint structure -- just a list (label offset absolute-offset)
(define (breakpoint-label breakpoint) (car breakpoint))
(define (breakpoint-offset breakpoint) (cadr breakpoint))
(define (breakpoint-absolute breakpoint) (caddr breakpoint))

;; labelsはextract-labelsで作った全label
(define (make-breakpoint label offset labels)
  (let ((record (assoc label labels)))
    (if record
      ;; calculate the absolute line number for this breakpoint
      ;; breakpoint単体の構成は(fact-loop 1 2)という形. offset=1,absolue=2
      (list label offset (+ offset (label-entry-count record)))
      (error "Unknown label -- MAKE-BREAKPOINT" label))))


;; [re-define]
(define (extract-labels text receive)
  ;; 反復させながらcount.
  (define (extract-labels-count count text receive)
    (if (null? text)
      (receive '() '())
      (extract-labels-count
        (if (symbol? (car text)) count (+ 1 count)) ; ++ labelはcountに含まない(+1をskip)
        (cdr text) ; 第二引数
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
              (receive insts
                       (cons (make-label-entry next-inst ; = label-name
                                               insts
                                               count) ; ++
                             labels))
              (receive (cons (make-instruction next-inst count) ; ++ count
                             insts)
                       labels)))))))
  (extract-labels-count 0 text receive))

;; [re-define]
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ;; install list of labels into machine
    ((machine 'set-labels) labels) ; ++ なぜだろう
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops)))
      insts)))


;; [re-define] 引数増えた.
(define (make-instruction text count)
  (print (list text '() count))
  (list text '() count)) ; cons -> list, 3要素目に行番号を保持する
;; instruction-text はcar取るだけなのでそのまま
(define (instruction-execution-proc inst)
  (cadr inst)) ; cdr -> cadr
(define (instruction-line-count inst) ; [new]
  (caddr inst))
(define (set-instruction-execution-proc! inst proc)
  ;; instのcdrではなく(cdr inst)のcdrへsetするよ
  ;; => 間違い. set-cdr!ではなくset-car!. ここミスってハマってた.
  (set-car! (cdr inst) proc))

;; [new]
(define (label-entry-name entry) (car entry))
(define (label-entry-inst entry) (cadr entry))
(define (label-entry-count entry) (caddr entry)) ; labelのcountはどうなる? +1してなかったけど

;; [re-define]
(define (make-label-entry label-name insts count) ; 引数count追加
  (list label-name insts count)) ; cons -> list, count追加

;; [re-define]
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (label-entry-inst val) ; instを取り出すために使う手続きがcdrからcadrへ.
      (error "Undefined label -- ASSENBLE" label-name))))



;; 動作テスト
(define fact-machine
  (make-machine
    '(n val continue)
    `((- ,-) (= ,=) (* ,*))
    '((assign continue (label fact-done))
      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done)))

(set-register-contents! fact-machine 'n 4)
(set-breakpoint! fact-machine 'fact-loop 1)

; (print-all-breakpoints fact-machine)
; ;; => ((fact-loop 1 2))#<undef>

 (start fact-machine)
;; ;   gosh>  (start fact-machine)
;; ;   (bp-abs= 2), linenum=0
;; ;   (bp-abs= 2), linenum=1
;; ;   (bp-abs= 2), linenum=2
;; ;   Breakpoint hit -- fact-loop 1
(get-register-contents fact-machine 'n) ; => 4
(get-register-contents fact-machine 'val) ; => *unassigned*
(get-register-contents fact-machine 'continue) ; => ()
(proceed-machine fact-machine)
;; 3->4->5->6->7->2 Breakpoint hit -- fact-loop 1
(get-register-contents fact-machine 'n) ; => 3
(get-register-contents fact-machine 'val) ; => *unassigned*
(get-register-contents fact-machine 'continue)
;; => (((restore n) #<closure (make-restore make-restore)> 8) ...(ry)... to make-goto)> 13))
(proceed-machine fact-machine)
;; ; (bp-abs= 2), linenum=4
;; ; (bp-abs= 2), linenum=5
;; ; (bp-abs= 2), linenum=6
;; ; (bp-abs= 2), linenum=7
;; ; (bp-abs= 2), linenum=1
;; ; (bp-abs= 2), linenum=2
;; ; Breakpoint hit -- fact-loop 1
(get-register-contents fact-machine 'n) ; => 2
(get-register-contents fact-machine 'val) ; => *unassigned*
(get-register-contents fact-machine 'continue) ; => (ry)
(proceed-machine fact-machine)
;; 3->4->5->6->7->1->2 Breakpoint hit -- fact-loop 1
(get-register-contents fact-machine 'n) ; => 1
(get-register-contents fact-machine 'val) ; => *unassigned*
(get-register-contents fact-machine 'continue) ; => (ry)
(proceed-machine fact-machine)
;; ; (bp-abs= 2), count=12
;; ; (bp-abs= 2), count=13
;; ; (bp-abs= 2), count=8
;; ; (bp-abs= 2), count=9
;; ; (bp-abs= 2), count=10
;; ; (bp-abs= 2), count=11
;; ; (bp-abs= 2), count=8
;; ; (bp-abs= 2), count=9
;; ; (bp-abs= 2), count=10
;; ; (bp-abs= 2), count=11
;; ; (bp-abs= 2), count=8
;; ; (bp-abs= 2), count=9
;; ; (bp-abs= 2), count=10
;; ; (bp-abs= 2), count=11
;; ; done
(get-register-contents fact-machine 'n) ; => 4 (初期値に戻っている...)
(get-register-contents fact-machine 'val) ; => 24 (解答)
(get-register-contents fact-machine 'continue) ; => ()

;; DEBUG(1) {{{
;; start machineしたタイミングでこんなエラー
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (filter (lambda (breakpoint) (= (breakpoint-absolute breakpoint) ( ...!!!
; !!!        At line 67 of "./q5.19.scm"!!!
; !!!!!!(start fact-machine)
; ((assign continue (label fact-done)) () . #<closure (make-assign make-assign)>)gosh>

; (define (make-instruction text count)
;   (print (list text '() count)) ; ++
;   (list text '() count))
; gosh> (load "./q5.19")
; ((goto (reg continue)) () 13)
; ((assign val (const 1)) () 12)
; ((goto (reg continue)) () 11)
; ((assign val (op *) (reg n) (reg val)) () 10)
; ((restore continue) () 9)
; ((restore n) () 8)
; ((goto (label fact-loop)) () 7)
; ((assign continue (label after-fact)) () 6)
; ((assign n (op -) (reg n) (const 1)) () 5)
; ((save n) () 4)
; ((save continue) () 3)
; ((branch (label base-case)) () 2)
; ((test (op =) (reg n) (const 1)) () 1)
; ((assign continue (label fact-done)) () 0)
;; ちゃんと入っとる気がするけど....
;; あ, cdrじゃなくcddrにsetしてしまっているのか？
;; => あたり. set-instruction-execution-proc! にミスがあった }}}

;; DEBUG(2): そもそもfilterが定義されてなかったのでsec2.2.3.scmからこぴぺ

