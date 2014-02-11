;; C-c C-z  switch-to-scheme
;; C-c C-e  scheme-send-definition
;; C-c C-r  scheme-send-region
;; C-c C-l  scheme-load-file

;; =============================================
;; p.200 対の無限のストリーム (問題3.65の次から)
;; =============================================

(load "../sec3/stream")

;; 2.2.3 では, 入れ子のループを対の並びで定義されたプロセスとして扱った. これを無限ストリームへ応用する.

;; 没案 stream-append
;;   (1 . 1), (1 . 2) ... (1 . 100) ... (1 . 9999999) ... と,
;;   最初の要素が1である組み合わせをまず全て網羅しようとしてしまう.
(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-append (stream-cdr s1) s2))))

;; 採用 interleave
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   ;; s1とs2を入れ替え, 順々に消費していく
                   (interleave s2 (stream-cdr s1)))))

;; pairs完成版
;; i <= j の条件
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave ;; *
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))


;; 問題 3.66
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(pairs integers integers) ;; の性質を調べる.

;; (take 100 (pairs integers integers))
;; (1 1),
;; (1 2), (2 2),
;; (1 3), (2 3),
;; (1 4), (3 3),
;; (1 5), (2 4),
;; (1 6), (3 4), ...

;; (1 j) が (最初を除いて) 1個おきに出ている.
;; 次に (1 . j) を取り除いてみると, s/(1 .+?), //g
;; (2 j) が1個おきに出ている. (元々のストリームから見るとn%4==0のとき)
;; 同様に(3 j) はn%8==0, (4 y) はn%16==0の項で出現しており, 一般化すると

;; (i j) = 2^(i-1) * (2 * (j - 1) - 1) 

(define (index i j)
  (cond ((and (= i 1) (= j 1)) 0) ;; 分解の第一部
        (else (* (expt 2 (- i 1)) (- (* 2 (- j 1)) 1)))))

;; (* (- (* 2 99) 1) (expt 2 98))
;; 62431792061240298023712632864768

;; (index 1 100)
;; 197
;; (stream-ref (pairs integers integers) 197)
;; (1 100)
;; (index 99 100)
;; 62431792061240298023712632864768
;; (index 100 100)
;; 124863584122480596047425265729536


(define (index-pairs x y)
  (cond ((> x y) (error "error x larger than y" x y))
        ((and (= x 0) (= y 0))
         0)
        ((and (>= x 1) (= x y))
         (+ (index-pairs (- x 1) (- y 1)) (expt 2 (- x 1))))
        ((= x 1)
         (* 2 (- y 1)))
        ((= x (- y 1))
         (+ (index-pairs x (- y 1)) (expt 2 (- x 1))))
        (else
         (+ (index-pairs x (- y 1)) (expt 2 x)))))

(define (hoge i n s) (if (equal? n (stream-car s))
                         (display i)
                         (hoge (+ i 1) n (stream-cdr s))))

;; 前に197個 1 origin

;; 問題 3.67
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; i<=j の条件を外したすべての (i, j) 対ストリームを生成する.

(define (all-pairs s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (interleave
      (stream-map
        (lambda (x) (list (stream-car s1) x))
        (stream-cdr s2))
      (interleave
        (stream-map
          (lambda (x) (list x (stream-car s2)))
          (stream-cdr s1))
        (all-pairs (stream-cdr s1) (stream-cdr s2))))))

;; (take 10 ss)
;; (1 1), (1 2), (2 1), (1 3), (2 2), (1 4), (3 1), (1 5), (2 3), (1 6), done


;; 問題 3.68
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 対ストリームを三個に分解して考えるよりも,
;; (S_0, T_0)を1行目全体に含めて次のように扱ったほうが単純であるという提案.

(define (louis-pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (louis-pairs (stream-cdr s) (stream-cdr t))))

;; => 動かない.
;;    cons-streamがdelay objに包んでいたinterleaveをむき出しにしてしまったため,
;;    無限ストリームを評価しようとして無限ループになる.


;; 問題 3.69
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; i<=j<=k となる (S_i, T_i, U_i) のstreamを生成するtripletsを定義して,
;; それを元にピタゴラス数の対ストリームを作る.

;; ref: 以下を満たす自然数の組をピタゴラス数という.
;;    i^2 + j^2 = k^2

(define (triplets s1 s2 s3)
  (cons-stream
    (list
      (stream-car s1)
      (stream-car s2)
      (stream-car s3))
    (interleave
      (stream-map
        (lambda (x) (append (list (stream-car s1)) x))
        (stream-cdr (pairs s2 s3)))
      (triplets
        (stream-cdr s1)
        (stream-cdr s2)
        (stream-cdr s3)))))

;; (take 10 (triplets integers integers integers))
;; (1 1 1), (1 1 2), (2 2 2), (1 2 2), (2 2 3), (1 1 3), (3 3 3), (1 2 3), (2 3 3), (1 1 4), done

(define (square x) (* x x))
(define pythagorean (stream-filter
                     (lambda (triplet) (= (square (caddr triplet))
                                          (+ (square (car triplet))
                                             (square (cadr triplet)))))
                     (triplets integers integers integers)))

;; (take 5 pythagorean) 5個, 少々時間かかる
;; (3 4 5), (6 8 10), (5 12 13), (9 12 15), (8 15 17), done


;; 問題 3.70
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 対が"ある有用な順"で現れるようなストリームを定義する.
;; 引数に与える weightは"重み関数"で, 何らかの基準で対に順序を付ける.
(define (merge-weighted weight s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let* ((s1car (stream-car s1))
             (s1w (weight s1car))
             (s2car (stream-car s2))
             (s2w (weight s2car)))
      (cond ((<= s1w s2w)
             (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
            (else
              (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2)))))))))

;; merge-weightedを使ってpairsをmergeするweighted-paris
(define (weighted-pairs weight s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (merge-weighted
      weight
      (stream-map
        (lambda (x) (list (stream-car s1) x))
        (stream-cdr s2))
      (weighted-pairs weight (stream-cdr s1) (stream-cdr s2)))))

;; (a). 和 i+j をweightとしたとき

(define sum-weighted-pairs (weighted-pairs (lambda (pair) (apply + pair))
                                           integers integers))

;; (take 10 sum-weighted-pairs)
;;    =>  (1 1), (1 2), (1 3), (2 2), (1 4), (2 3), (1 5), (2 4), (3 3), (1 6), done
;; pairs: (1 1), (1 2), (2 2), (1 3), (2 3), (1 4), (3 3), (1 5), (2 4), (1 6), done

;; (1 1), (1 2), (1 3), (2 2), (1 4), (2 3), (1 5), (2 4), (3 3), (1 6), (2 5), (3 4), (1 7), (2 6), (3 5), (4 4), (1 8), (2 7), (3 6), (4 5), done
;; (1 1), (1 2), (2 2), (1 3), (2 3), (1 4), (3 3), (1 5), (2 4), (1 6), (3 4), (1 7), (2 5), (1 8), (4 4), (1 9), (2 6), (1 10), (3 5), (1 11), done


;; (b). 和 2i+3j+5ij をweightとしたとき, iもjも2,3,5で割り切れない正の整数対(i, j)のストリーム

;; まず2,3,5で割り切れない数のstreamを生成する
(define (divides? a b) (= (remainder b a) 0))
(define no-235-factors (stream-filter
                         (lambda (n) (not (or (divides? 2 n)
                                              (divides? 3 n)
                                              (divides? 5 n))))
                         integers))

;; それを 2i+3j+5ij で重み付けする
(define weighted-no-235-factors
  (weighted-pairs
   (lambda (pair) (+ (* 2 (car pair))  ;; 2i
                     (* 3 (cadr pair)) ;; 3j
                     (* 5 (car pair) (cadr pair)))) ;; 5ij
   no-235-factors
   no-235-factors))

;; (take 10 weighted-no-235-factors)
;; (1 1), (1 7), (1 11), (1 13), (1 17), (1 19), (1 23), (1 29), (1 31), (7 7), done


;; 問題 3.71
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ramanujan数(別名: タクシー数) -- 二通り以上の, 二つの立方数の和で表される数. つまり
;;     n^3 + m^3 = X であり, かつ
;;     s^3 + t^3 = X である
;; ようなn,m,s,tが存在するXのことを言う.

;; 最初のRamanujan数は1729である.
;;   Ta(2) = 1729 = 1^3 + 12^3 = 9^3 + 10^3 (n, m, s, t = 1, 12, 9, 10)

;; まず和 i^3 + j^3 の重みで順序付けられたストリームを生成して,
;; 次に, そこから同じ重みで2個(以上)連続するpairを探す.

(define (cube x) (* x x x))
(define (cube-weight pair) (+ (cube (car pair)) (cube (cadr pair))))
(define cube-sums (weighted-pairs cube-weight integers integers))

(define (ramanujan s)
  (let ((s1car (stream-car s))
        (s2car (stream-car (stream-cdr s))))
    (let ((s1-weight (cube-weight s1car))
          (s2-weight (cube-weight s2car)))
      (if (= s1-weight s2-weight)
        (cons-stream s1-weight
                     (ramanujan (stream-cdr s)))
        (ramanujan (stream-cdr s))))))

;; (take 2 (ramanujan cube-sums))
;; 1729, 4104, done


;; 問題 3.72
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3.71と似た方法で, 3通りの異なる方法で2つの平方数の和として書けるすべての数のストリームを生成する.
;;   X = n^2 + m^2
;;     = s^2 + t^2
;;     = x^2 + y^2
;;  となるn,m,s,t,x,yが存在するようなXを探す.

(define (square-weight pair)
  (+ (square (car pair)) (square (cadr pair))))
(define squares (weighted-pairs square-weight integers integers))

(define (ramanujan3 s)
  (let ((s1car (stream-car s))
        (s2car (stream-car (stream-cdr s)))
        (s3car (stream-car (stream-cdr (stream-cdr s)))))
    (let ((s1-weight (square-weight s1car))
          (s2-weight (square-weight s2car))
          (s3-weight (square-weight s3car)))
      (if (= s1-weight s2-weight s3-weight)
        (begin
          ;; debug print
          (newline)
          (display s1car)
          (display s2car)
          (display s3car)
          (display " - ")
          (cons-stream s1-weight
                       (ramanujan3 (stream-cdr s))))
        (ramanujan3 (stream-cdr (stream-cdr s)))))))

;; TODO: 325も入る

;; (take 10 (ramanujan3 squares))
;;    (5 25) (11 23) (17 19)  650,
;;    (7 26) (10 25) (14 23)  725,
;;    (1 32)  (8 31) (20 25) 1025,
;;    (4 33)  (9 32) (12 31) 1105,
;;    (9 32) (12 31) (23 24) 1105,
;;   (10 35) (13 34) (22 29) 1325,
;;    (1 38) (17 34) (22 31) 1445,
;;    (5 40) (16 37) (20 35) 1625,
;;   (16 37) (20 35) (28 29) 1625,
;;   (12 41) (15 40) (23 36) 1825,



;; =============================================
;; p.203 信号としてのストリーム
;; =============================================

;; NOTE: このintegralはsec3.5.4で被積分ストリームを遅延引数として扱うように再定義される.
(define (integral integrand initial-value dt)
   (define int (cons-stream initial-value
                            (add-streams (scale-stream integrand dt)
                                         int)))
   int)

;; 図3.32: 入力ストリームはdt倍されてadd-streamで自身と加算される.
;;         フィードバックループはint定義内にintが含まれることを表す.


;; 問題 3.73
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ストリームを使って電流/電圧の時系列値を表現することで, 電気回路をモデル化する.
;; 抵抗値Rの抵抗と容量Cのコンデンサが直列になったRC回路(図3.33)を考える.
;; 注入電流iに対する回路の電圧応答vの関係.

;; RCに抵抗値R, 容量C, dtを渡すと,
;; "電流ストリームと初期電圧値を引数に取り, 電圧のストリームを返すような手続き" を返す.
(define (RC R C dt)
  (lambda (i-stream v0) (add-streams
                          (integral (scale-stream i-stream (/ 1 C)) v0 dt)
                          (scale-stream i-stream R))))

(define RC1 (RC 5 1 0.5)) ;; RC1は手続き.
;; (take 10 (RC1 integers 1))
;; 6, 11.5, 17.5, 24.0, 31.0, 38.5, 46.5, 55.0, 64.0, 73.5, done

(define cos-stream (stream-map (lambda (x) (cos x))
                               (scale-stream integers 0.2)))

;; 問題 3.74
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 零交差(zero-crossings)を表すシステムを作る.
;; 信号(stream)を渡すと, その信号が+から-になったとき-1, -から+になったとき+1, それ以外は0であるようなstreamを返す.

;; Alyssaの作った版
;; last-valueを保持しながら再帰的にdetectorを作用させ, 出力ストリームを作る.
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

;; こうして使う.
;; (define zero-crossings (make-zero-crossings sense-data 0))

;; q3.50.scm のstream-mapを利用できるとのこと.

;; sign-change-detector は引数として2つの値を取り, 0, -1, +1のいずれかを返す。
(define (sign-change-detector current last)
  (cond ((and (> last 0) (<= current 0)) -1)
        ((and (< last 0) (>= current 0))  1)
        (else  0)))

;; (sin 1), (sin 2), (sin 3),... なsense-dataを作る.
(define sense-data (stream-map (lambda (x) (sin x)) integers))

;; (take 10 sense-data)
;; 0.8414709848078965, 0.9092974268256817, 0.1411200080598672, -0.7568024953079282, -0.9589242746631385,
;; -0.27941549819892586, 0.6569865987187891, 0.9893582466233818, 0.4121184852417566, -0.5440211108893699, done

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; (take 10 zero-crossings)
;; 0, 0, 0, -1, 0, 0, 1, 0, 0, -1, done


;; 問題 3.75
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sense-dataの質が悪くノイズが発生する.
;; Louisが実装した平滑化プログラムは以下のとおりだが, バグがあるので修正せよ.

;; (define (make-zero-crossings input-stream last-value)
;;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;;     (cons-stream (sign-change-detector avpt last-value)
;;                  (make-zero-crossings (stream-cdr input-stream)
;;                                       avpt))))

;; last-avptも引数に渡す.
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))


;; (define zero-crossings (make-zero-crossings sense-data 0 0))

;; 平滑化の効果(前回書いたグラフ)
;; https://www.evernote.com/shard/s11/sh/388fd5b5-c2a7-4598-a819-37e1c8ab4af0/d8f7b63cd7e6529f3c2c602572d85394


;; 問題 3.76
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 零交差と平滑化部分を分離したい.
(define (smooth stream)
  (if (or (stream-null? stream)
          (stream-null? (stream-cdr stream)))
    the-empty-stream
    (stream-map
      (lambda (x1 x2) (exact->inexact (/ (+ x1 x2) 2)))
      stream
      (stream-cdr stream))))

;; (take 10 sense-data)
;; 0.8414709848078965, 0.9092974268256817, 0.1411200080598672, -0.7568024953079282, -0.9589242746631385,
;; -0.27941549819892586, 0.6569865987187891, 0.9893582466233818, 0.4121184852417566, -0.5440211108893699
;; (take 10 (smooth sense-data))
;; 0.8753842058167891, 0.5252087174427744, -0.3078412436240305, -0.8578633849855333, -0.6191698864310322,
;; 0.1887855502599316, 0.8231724226710855, 0.7007383659325692, -0.06595131282380665, -0.7720056587200367

;; q3.74.scm のstream-map版と組み合わせる.
(define zero-crossings (stream-map sign-change-detector (smooth sense-data)
                                                        (cons-stream 0 (smooth sense-data))))

;; (take 10 zero-crossings)
;; 0, 0, -1, 0, 0, 1, 0, 0, -1, 0, done

