(load "./sec4.2-lazy")

;; (1). memo化すると高速化するプログラムを示せ.
;; 素数判定とかフィボナッチ? 昔やったはず...

;; 時間の計測をどうする? いろいろ検討
;; fの中にcounterを入れようとしたがこれだと結局forceしないとカウントされないので一緒.
; (begin (display "fib: ") (display (f 5)) (newline)
;        (display "count: ") (display c))

;; このように#?= でdebug printさせてカウントとか => 手動ちょっとな.
;   (define (actual-value exp env)
;     (force-it #?=(eval exp env)))

;; gauche.timeを使う. これでいこう. 入れる場所悩んだけどここがよさげ.
; (use gauche.time)
; (define (driver-loop)
;   ...
;   (let ((output (time (actual-value input the-global-environment))))
;; driver-loopのエバるところにtimeをかます.

;; w/とw/oの切り替えは メモなしforce-it <=> メモ付きforce-it + evaluated-thunk組 のコメントアウト
(driver-loop)

(define (f n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (f (- n 1)) (f (- n 2))))))

(f 20)
;; w/o memo:
; => ;;; L-Eval input:
;    (f 20)
;    !!!;(time (actual-value input the-global-environment))!!!
;    !!!; real   7.310!!!
;    !!!; user   7.120!!!
;    !!!; sys    0.010!!!
;    !!!!!!

;; w/ memo:
; => ;;; L-Eval input:
;    (f 20)
;    !!!;(time (actual-value input the-global-environment))!!!
;    !!!; real   1.572!!!
;    !!!; user   1.470!!!
;    !!!; sys    0.000!!!

;; 7.3秒 -> 1.6秒. 圧倒的ですね



;; (2). 次の対話を考えよ.
;; q4.27.scm で定義したid.

(driver-loop)

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))


(square (id 10))
;;; L-Eval value => 100 (both)
count
;;; w/o memo: L-Eval value => 2
;;; w/  memo: L-Eval value => 1

