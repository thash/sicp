

(define (query-driver-loop)
  ;...
   (qeval q (singleton-stream '())))

x: hoge
y: futa
; という束縛があったとして, これをもとに
(foo ?x ?y)
;を
(foo hoge fuga)
; に置き換えるのがinstantiate.


;; (and (foo ?x ?y) (bar ?z))
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))
;; まず最初の(foo ?x ?y)を満たす可能性を列挙して,
;; その中からさらに (bar ?z) を満たすものを探す

;; これを
(put 'and 'qeval conjoin)
;; こんな感じに格納するんだけど第二の'qevalってなんだっけ. 二次元テーブルの2個目軸.

;; or = disjoin.
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
        (qeval (first-disjunct disjuncts) frame-stream)
        (delay (disjoin (rest-disjuncts disjuncts)
                        frame-stream)))))
 ;; interleave は「第一引数のstreamが無限だったらそこの可能性を探して永遠に終わらない」可能性を回避するため交互に取っていく手続き. そのdelay版


;; lisp-value
(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
            ;; その時点で確定しているものを当てはめていってexecute = lispとして評価する.
            (if (execute
                  (instantiate ;; qevalでも出てきたinstantiate
                    call
                    frame
                    (lambda (v f)
                            (error "Unknown pat var -- LISP-VALUE" v))))
                (singleton-stream frame) ;; 与えられたframeをsingleton繰り返ししたstream?
                the-empty-stream))
    frame-stream))

;; evalじゃなくexecuteである理由は?
(define (execute exp)
  (apply (eval (predicate exp) (interaction-environment))
         (args exp)))
;; 評価するときに与える環境, Gaucheではinteraction-environmentにしないと動かないよと


;;; 4.4.4.3 パターンマッチ

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                          (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
          (pattern-match query-pat assertion query-frame)))
       (if (eq? match-result 'failed) ;; failedをemptyに変換してる
           the-empty-stream
           (singleton-stream match-result))))


(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame) ;; patternとdataが一致すれば(これがpatternmatchの本体(?)) frameを返す.
        ((var? pat) (extend-if-consistent pat dat frame))
        ;; extend-if-consistentは整合性があるかどうかチェックして, 整合性が取れてたらフレームを拡張する.
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed))) ;; 最後までなければ'failedを返す(で, check-an-assertionにてthe-empty-streamに置き換えられる.)

;; pattern-matchをそのままで使いたい！
;; query-driver-loopでassertした後エラー起こして(適当な数値を出す)goshに戻ってきて以下を評価すればいい. そんな方法が.
(pattern-match '(job (? x) (? y)) '(job foo bar) the-empty-stream)
(((? y) . bar) ((? x) . foo))


(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
       (cond (binding
               (unify-match
                 (binding-value binding) val frame))
             ((var? val) ; ***
              (let ((binding (binding-in-frame val frame)))
                   (if binding
                       (unify-match
                         var (binding-value binding) frame)
                       (extend var val frame))))
             ((depends-on? val var frame) ; ***
              'failed)
             (else (extend var val frame)))))

;; extendはgaucheに既にあるので(ゆえにsyntax highlight)

;; シンプルなprologならここまででok, とのこと.

;;;; 4.4.4.4 規則とユニフィケーション
;; uniqueなidを作る
;; (? x) 1回目
(? 1 x)
;; (? x) 2回目
(? 2 x)


(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
       (let ((unify-result
               (unify-match query-pattern
                            (conclusion clean-rule)
                            query-frame)))
            (if (eq? unify-result 'failed)
                the-empty-stream
                (qeval (rule-body clean-rule)
                       (singleton-stream unify-result))))))


;; ここがいよいよunifyの本体
;; pattern-matchと似てるがp1,p2とふたつ変数入る可能性があるのが大きな違い.
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

;; extend-if-possibleとは?
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
       (cond (binding
               (unify-match
                 (binding-value binding) val frame))
             ((var? val) ; ***
              (let ((binding (binding-in-frame val frame)))
                   (if binding
                       (unify-match
                         var (binding-value binding) frame)
                       (extend var val frame))))
             ((depends-on? val var frame) ; ***
              'failed)
             (else (extend var val frame)))))

;; "y"と"yを含む式"はマッチできない, それを判定するのがdepends-on?
(unify-match '(job (? x) (? y)) '(job (? z) (computer (? y))) the-empty-stream)
;; うまくうごかん


; ----------------------------------------------------------------------------------

;;;; 疑問
;; ? (quit!) はなにしてる？
;; ? prologなの?
;; ? idを付与する的なところ, uniqueじゃないと困る理由聞き逃した


;; ? (quit!) はなにしてる？
;;  => 発表者が独自に定義してた. THE-ASSERTIONSとかTHE-RULESをthe-empty-streamにいれたうえで'quit. で脱出


; delayしておけば何かしら印字されるけどdelaysしてなければぜんぶ
; 単純なるーぷであればstackに積んでおいてもっかいきたときに覚えておいて判定すれば良いよ


;; flatten-streamはflatにしようとしてる.
;; flatten-stream は
((stream1) (stream1) ...)
;; となっているものを
(stream1 stream1 ...)
;; にしてやる.
;;
;; で, streamに無限があればいつまでもflatできないので差し込みします. interleave.

;; で, そもそも
((stream1) (stream1) .. "ココ" .)
;; が無限に続いているかもしれないのでdelayを入れないといけない.




