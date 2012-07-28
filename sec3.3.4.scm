;; 3.3.4. ディジタル回路のシミュレーター
;;
;;    機能箱(function boxes)
;;     inverter: 入力を反転する
;;     and-gate: logical and. 1 + 1 => 1, 1 + 0 => 0
;;     or-gate: logical or. 1 + 0 => 1, 0 + 0 => 0

;; 半加算器(half-adder)を作ろう。
;;   [図]
;;
;; 入力と出力のパターン
;;   * A:1,B:1 => D:1,E:0 => S:0,C1
;;   * A:0,B:0 => D:0,E:1 => S:0,C0
;;   * A:1,B:0 => D:1,E:1 => S:1,C1
;;   * A:0,B:1 => D:1,E:1 => S:1,C1
;;
;; 状態遷移図は以下のようになる
;;   [図]

;; まず作り方の方針を検討する。
;;  回線(wires)をmake-wire手続きで構成.
;; (define a (make-wire))
;; (define b (make-wire))
;; (define c (make-wire))
;; (define d (make-wire))
;; (define e (make-wire))
;; (define s (make-wire))
;;
;; ;; function boxの各手続きで表現すると、
;; (or-gate a b d)
;; (and-gate a b c)
;; (inverter c e)
;; (and-gate d e s)
;;
;; ;; これを組み合わせればhalf-adderが表現できる。
;; (define (half-adder a b s c)
;;   (let ((d (make-wire)) (e (make-wire)))
;;     (or-gate a b d)
;;     (and-gate a b c)
;;     (inverter c e)
;;     (and-gate d e s)
;;     'ok))
;;
;; ;; これで、half-adderを構成要素として利用した全加算器も作れる。
;; (define (full-adder a b c-in sum c-out)
;;   (let ((s (make-wire))
;;         (c1 (make-wire))
;;         (c2 (make-wire)))
;;     (half-adder b c-in s c1)
;;     (half-adder a s sum c2)
;;     (or-gate c1 c2 c-out)
;;     'ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 実際に作って行く。まず基本的なfunction boxesを作るわけだが、その前にwireを具体化する必要がある。
;; 本文の記述をヒントに、wireのモデルを構築する.
;; wireは get-signal, set-signal!, add-action!手続きを受け取らなければならない。
;; 内部に状態を持ち、状態を参照/変更させながらさまざまな手続きを受け取る。これはdispatchの出番。
(define (make-wire)
  ;; wireの持つデフォルトsignal値は0.
  (let ((signal 0) (procs '()))

    ;; 内部手続き
    (define (add-action! proc)
      (set! procs (cons proc procs)))

    (define (_set-signal! new-value)
      (if (not (= new-value signal))
        (begin (set! signal new-value)
               (call-procs procs))))

    (define (call-procs procs)
      (if (null? procs)
        (begin
          (set! procs '()) ;;これはいらない？
          'done-calling-procs.)
        (begin
          ((car procs))
          (call-procs (cdr procs)))))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) _set-signal!)
            ((eq? m 'add-action!) _add-action!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; add-action!の意義がよくわからなかったが、
;; 要するにwireにおいてはinputに対して「なんか」して出力が出てくるのだけど,consにprocをqueueしていってsignalをsetする段階で「一気になんかする」.
;; stackを積むのがadd-action!. それを崩して実行するのがcall-procs.


;; ようやくfunction boxes. 彼らは直接signalの値を操作しないことに着目。
;; orは |q3.28.scm| で実装
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal -- LOGICAL-NOT" s))))

(define (logical-and a1 a2)
  (cond ((and (= a1 1) (= a2 1)) 1)
        ((or  (= a1 0) (= a2 0)) 0)
        (else (error "Invalid signal -- LOGICAL-AND" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

