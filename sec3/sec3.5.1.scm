;;; 3.5. ストリーム
;;
;;   代入によって引き起こされた問題を反省し、別の方法で回避できないか考える。
;;   時の経過をモデル化に取り込んだのが悪かったのではないか。
;;   時による変化はx(t)と表せる。個々のtに着目すればたしかに状態は変化するが、x(t)という関数自体は不変である。
;;
;;   > 時を離散ステップで測るなら、時間関数を(無限かもしれぬ)並びとしてモデル化できる。
;;   > 本節ではモデル化しようとするシステムの時間史を表現する並びを使い、変化をモデル化する方法を見よう。
;;
;;   そこで「ストリーム(stream)」という新しいデータ構造を取り入れる。
;;   ストリームを使えば、代入や可変データを使わずに、状態を持つシステムをモデル化できる。
;;     # Gaucheでは(use util.stream)で使える。
;;   ストリームを(sec2.2.1のように)単なるリストとして表現するのではなく、遅延評価(delayed evaluation)の技法を取り入れる。


;;; 3.5.1. ストリームは遅延リスト

;; (a). 標準的な反復
;; aからbまでの整数から素数を探して足し合わせる.
;; prime? は sec1/q1.21.scm から.
;; e.g. (sum-primes 0 10) => 17
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

;; (b). 2.2.3の並びの演算
;; 同じロジックを並びで実現する.
;; enumerate-intervalは ./sec2/sec2.2.3.scm から.
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;; (a)で必要なのは累積する合計を格納する場所のみ。
;; (b)はfilterがenumerate-interval区間内の完全なリストを構築するまで何も出来ない。大きな中間構造が必要になる。

;; より非効率さが分かるのは
(car (cdr (filter prime?
                  (enumerate-interval 10000 10000000))))
;; などとしたとき。100万個近い整数のリストを作り、すべて素数性をテストして素数リストを作り、そっからcdrしてcarする。
;; ほとんど必要が無いデータを作っており、オーバーヘッドがひどい。
;;
;; そこでストリームですよ。
;; ストリームがあれば、並びとして評価するコストを追うことなく、並びに対する演算を行うことが出来る。
;;
;; まずはSICPお馴染み、「使い方」から考えていく. stream-carとstream-cdr, cons-streamの定義は少し後.

(stream-car (cons-stream x y)) = x
(stream-cdr (cons-stream x y)) = y

(the-empty-stream (stream-null?))

;; streamから任意のindexの要素を取得するstream-ref.
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

;; sec3/q3.50.scm で改良される. コレは非効率版.
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


;; 遅延評価のキモは、cons-streamでストリームが構成されたとき「ではなく」
;; stream-cdrでアクセスされたときにcdrを評価するようにすることである。
;;
;; そこで特殊形式delayを使う。(delay <exp>)は式<exp>を評価せず、遅延オブジェクト(delayed object)を返す。
;; 遅延オブジェクトは(force <delayed obj>)としてforceを作用させることで評価を実行するものとする。
;;   # delayとforce, syntax highlightされるな。
;; 肝心のforce, delayの実装はまた後回しにして、cons-streamとstream-cdrは次のように実装される。

(cons-stream <a> <b>)
;; は、以下のようになる。
(cons <a> (delay <b>))

;; そしてcarとcdrはこうなる。
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; 空のstreamなどを定義.
(define the-empty-stream '())
(define stream-null? null?)

;;; delayとforceの実装 ;;;
;; (delay <exp>)は(lambda () <exp>)のようなsyntax sugarとして考える。
;; delayed-objectは式をパッケージ化する。
;;   # lispだからできること？
;; するとforceは

(define (force delayed-object)
  (delayed-object))

;; さらに、一度forceした遅延オブジェクトの計算結果を格納するような作りにする。要はメモ化である。

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))

;; これを使ってdelayを定義し直す。forceはそのまま。
(define (delay <exp>)
  (memo-proc (lambda () <exp>)))

;; 実際に動くようにするにはdefine-macroでマクロとして定義してやる必要がある。
;;   (と書くと難しそうだが, いちばん素のdelayはlambdaの単なるsyntax sugar, という話)
;; memo化しないバージョンのdelay
(define-macro (delay x) `(lambda () ,x))
;; memo化バージョンのdelay
(define-macro (delay x) `(memo-proc (lambda () ,x)))

;; gosh> (define a (delay 'aaa))
;; gosh> a ;;=> #<closure a>
;; gosh> (force a) ;;=> aaa

;; 最終的にこれらを使って実装したcons-streamは
(define-macro (cons-stream a b)
  `(cons ,a (delay ,b))) ;; delayで包まれてるからbをevalしちゃっていい.


;; cons-streamを使って整数の無限ストリームなんかを作っていく例は => ./sec3/sec3.5.2.scm へ

