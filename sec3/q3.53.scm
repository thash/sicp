(load "./sec3.5.2")
;; プログラムを走らせずに
(define s (cons-stream 1 (add-streams s s)))
;; で定義するストリームの要素を述べよ.

;; add-streamsは何をしているものだったか
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; stream-map(任意個引数ver)は何をしているものだったか
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

;; 問題3.50でやっとけよ感あるが、いまさらstream-mapの動きを解説する。
;; (map stream-car argstreams) は、argstreams(1個以上のstreamたち)それぞれに対してstream-carを行いその結果をリストで返す。
; gosh> (map (lambda (x) (* x x)) '(1 2 3))
; (1 4 9)
; d => (1 4 9) is an instance of class <pair>

;; 次にapplyそのものは misc/apply.scm にまとめたような動きをする。
; http://practical-scheme.net/gauche/man/gauche-refj_56.html
; > Function: apply proc arg1 … args
; >    [R5RS] (arg1 … . args)を引数として手続きprocを呼びます。 最後の引数argsは正規のリストでなければなりません。 procが返す 値をそのまま返します。
;
; >    (apply list 'a 'b '(c d e)) ⇒ (a b c d e)
; >    (apply + 1 2 '(3 4 5))      ⇒ 15
;
;; (proc ...) としない理由はおそらくargstreamsの個数が不定なので汎用的なapplyを使ったんじゃないかな。
;; ここまでわかれば最後の
;      (apply stream-map
;             (cons proc (map stream-cdr argstreams)))
;; この式もやっていることがわかる。applyはstream-mapを実行したい。stream-mapに渡す引数としてリストが必要だから、consでprocと"argstreamsたちの一律cdr"というリストをくっつけているだけ。


;; 問題の"s"に戻ると、同じ要素をadd-streamしているのだから実質2倍2倍...と続いていくことになる。
;;  要するに2^nが正解。

