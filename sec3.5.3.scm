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
(display-stream-n (sqrt-stream 2) 10)
; 1.0
; 1.5
; 1.4166666666666665
; 1.4142156862745097
; 1.4142135623746899
; 1.414213562373095
; 1.414213562373095
; 1.414213562373095
; 1.414213562373095

;; 同様にPIも収束させるよ
;     PI/4 = 1 - 1/3 + 1/5 - 1/7 + ...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands .....TODO))))

;; 正負が交代するような級数は "euler-transform" を使って加速できる。
;; さらに加速されたストリームもストリームなので、加速加速加速できる


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 対の無限ストリーム
;;   # 1つのファイルに長々と書いていって結果出力をcomment outしながら説明していくのいいな

;; i+jが素数であるようなstreamを作ろう。
;; int-pairsはi <= jな整数(i,j)のすべての対の並びとすれば、次のようにfilterすればよい。
(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

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

;; こんなん考えてみましたが、s2から
..TODO

;; <conbine-in-some-way>候補そのに



