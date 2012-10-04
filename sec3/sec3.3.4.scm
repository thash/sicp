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
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;; function boxの各手続きで表現すると、
(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)
;;
;; これを組み合わせればhalf-adderが表現できる。
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; これで、half-adderを構成要素として利用した全加算器も作れる。
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 実際に作って行く。まず基本的なfunction boxesを作るわけだが、その前にwireを具体化する必要がある。
;; 本文の記述をヒントに、wireのモデルを構築する.
;; wireは get-signal, set-signal!, add-action!手続きを受け取らなければならない。
;; 内部に状態を持ち、状態を参照/変更させながらさまざまな手続きを受け取る。これはdispatchの出番。

(define (make-wire)
  (let ((signal-value 0) (action-procedure '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedure))
        'done))

    (define (accept-action-procedure! proc)
      (set! action-procedure (cons proc action-procedure))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; > 時と共に変わる信号を持ち、順次に装置に接続される回線は可変オブジェクトの代表である。われわれはそれを、代入で修正する局所状態を持つ手続きとしてモデル化した。


;; {{{2 勝手に編集したmake-wire
(define (make-wire-mod)
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
;; }}}2

;; add-action!の意義がよくわからなかったが、
;; 要するにwireにおいてはinputに対して「なんか」して出力が出てくるのだけど,consにprocをqueueしていってsignalをsetする段階で「一気になんかする」.
;; stackを積むのがadd-action!. それを崩して実行するのがcall-procs.


;; ようやくfunction boxes. 彼らは直接signalの値を操作しないことに着目。
;; function boxesの中身は「手続きを渡す手続き」.
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
        (else (error "Invalid signal -- LOGICAL-AND" a1 a2))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agenda データ構造を使ってafter-delayを実装
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay  5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'sum sum)


;; p.167...
;; agenda (次第書き)の各時間区分で走るべき手続きはqueueになっている。
;; agendaのデータ構造: http://www.serendip.ws/archives/1388

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg)))))

