;;; 『プログラミングGauche』 p.251
;; <generic>はGauche組み込みクラス。
(define-class <logger-generic> (<generic>)
              ())

;; logger-genericクラスのインスタンスとして総称関数を定義する
(define-generic add :class <logger-generic>)

(define-method apply-generic ((gf <logger-generic>) args)
               (format #t "args: ~s\n" args)
               (let ((return-value (next-method)))
                 (format #t "result: ~s\n" return-value)
                 return-value))

(define-method add ((num1 <number>) (num2 <number>))
               (+ num1 num2))

;; 与えられた引数と評価結果を出力するaddが定義できた。
;; (add 3 4)
;; => args: (3 4)
;; => result: 7

;;;;;;;;;;;;;;;;;;;;; time ;;;;;;;;;;;;;;;;;;;;;
(use gauche.time)

;; 先ほど作成したlogger-genericを継承してやる。
(define-class <profiler-generic> (<logger-generic>)
              ((counter :init-value 0)
               (time    :init-form (make <real-time-counter>))))

;; 総称関数subを定義
(define-generic sub :class <profiler-generic>)

(define-method apply-generic ((gf <profiler-generic>) args)
               (inc! (ref gf 'counter))
               (with-time-counter (ref gf 'time) (next-method)))
;; ここで使ったwith-time-counterはtime-counter-start!とtime-counter-stop!をまとめてやってくれる構文

(define-method sub ((num1 <number>) (num2 <number>))
               (- num1 num2))

;; 現在までのprofileを表示するメソッドと初期化を行うメソッド
;; ~sとか~dはprintfの%s, %dに相当するようだ
(define-method get-profile ((gf <profiler-generic>))
               (format #t "~s: ~d times called and spent time ~d\n"
                       (ref gf 'name) (ref gf 'counter) (time-counter-value (ref gf 'time))))

(define-method init-profile ((gf <profiler-generic>))
               (set! (ref gf 'counter) 0)
               (set! (ref gf 'time) (make <real-time-counter>)))

;; 使ってみましょう.
(use srfi-1)
(iota 10000 100 3)
;; なにこのモジュール便利

(map sub (iota 10000 100 3) (iota 10000 300 5))
(get-profile sub)
;; sub: 10000 times called and spent time 0.18861000000001918
;; まあこれ実際はlog出力が時間食ってる、ということで本文ではさらに改良を加える。



