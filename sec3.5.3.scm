;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.3. ストリームパラダイムの開発
(load "./stream")
(load "./my_defs")

;; 反復をストリームプロセスとして形式化する
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; squrt 2 に収束させる
;(display-stream-n (sqrt-stream 2) 10)
;; 1.0
;; 1.5
;; 1.4166666666666665
;; 1.4142156862745097
;; 1.4142135623746899
;; 1.414213562373095
;; 1.414213562373095
;; 1.414213562373095
;; 1.414213562373095

;; 同様にPIも収束させるよ
;     PI/4 = 1 - 1/3 + 1/5 - 1/7 + ...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

;; partial-sums from q3.55.scm
(define (partial-sums s)
  (cons-stream
    (stream-car s)
    (add-streams
      (stream-cdr s)
      (partial-sums s))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;(display-stream-n pi-stream 10)
;; 4.0
;; 2.666666666666667
;; 3.466666666666667
;; 2.8952380952380956
;; 3.3396825396825403
;; 2.9760461760461765
;; 3.2837384837384844
;; 3.017071817071818
;; 3.2523659347188767

;; この近似は緩やかに収束する。
; 100 => 3.1516934060711166
; 200 => 3.146617747495458


;; 正負が交代するような級数は "euler-transform" を使って加速できる。実はEulerの名が付くのは珍しいらしいが。
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; S_(n-1)
        (s1 (stream-ref s 1))  ; S_n
        (s2 (stream-ref s 2))) ; S_(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; 収束が速いよ!
;; (display-stream-n (euler-transform pi-stream) 10)
; 3.166666666666667
; 3.1333333333333337
; 3.1452380952380956
; 3.13968253968254
; 3.1427128427128435
; 3.1408813408813416
; 3.142071817071818
; 3.1412548236077655
; 3.1418396189294033

;; さらに加速されたストリームもストリームなので、加速加速加速できる


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 対の無限ストリーム
;;   # 1つのファイルに長々と書いていって結果出力をcomment outしながら説明していくのいいな

;; i+jが素数であるようなstreamを作ろう。
;; int-pairsはi <= jな整数(i,j)のすべての対の並びとすれば、次のようにfilterすればよい。

;     (stream-filter (lambda (pair)
;                      (prime? (+ (car pair) (cadr pair))))
;                    int-pairs)

;; ふたつのstream S = (S_i), T = (T_j)を考え、無限の正方配列を作る。
;; 正方配列の定義を再帰的に捉えると... => [図]

;    (define (pairs s t)
;      (cons-stream
;        (list (stream-car s) (stream-car t))
;        (<conbine-in-some-way>
;          (stream-map (lambda (x) (list (stream-car s) x))
;                      (stream-cdr t))
;          (pairs (stream-cdr s) (stream-cdr t)))))

;; ↑の<conbine-in-some-way>...内部ストリームを組み合わせる何らかの方法を考えることがキモになる。
;; <conbine-in-some-way>候補そのいち...
(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-append (stream-cdr s1) s2))))

;; こんなん考えてみましたが、s2を取り込む前にs1をすべての要素を取るので、無限streamには使えない。
;; (1 . 1), (1 . 2) ... (1 . 100) ... (1 . 9999999) ... と、最初の要素が1である組み合わせをまず全て網羅しようとする。
;; そこで、無限streamを使いつつ目的を達成するには、対を生成する並び順をちゃんと考えないといけない。
;; <conbine-in-some-way>候補そのに -- interleave
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))
;; 最後の行に注目。s1とs2を入れ替え、順々に消費していくstreamを作っている！
;; これでpairsの完成形ができる。
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

;; この発想から、いくつか並び順と要素の網羅性についての問題が続く。



