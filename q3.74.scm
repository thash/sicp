;; 信号処理, zero-crossings. 信号(stream)を渡すと, その信号が+から-になったとき-1, -から+になったとき+1, それ以外は0であるようなstreamを返す.

; (define (zero-crossings stream)
;   (...))
;
; というイメージかな?
; 問題文は続く. Alyssaたんは以下のような仕組みを考えたらしい.
; sense-dataは入力のstream.
;
;     (define (make-zero-crossings input-stream last-value)
;       (cons-stream
;         (sign-change-detector (stream-car input-stream) last-value)
;         (make-zero-crossings (stream-cdr input-stream)
;                              (stream-car input-stream))))
;
;     (define zero-crossings (make-zero-crossings sense-data 0))
;
; ところが彼女の上司によれば |q3.50.scm| のstream-map(一般化ver)を利用できるとのこと. これを完成させよ.
(load "./sec3.5.3")

;; sign-change-detector は引数として2つの値を取り, 0, -1, +1のいずれかを返す。
(define (sign-change-detector current last)
  (cond ((and (> last 0) (<= current 0)) -1)
        ((and (< last 0) (>= current 0))  1)
        (else  0)))

;; sense-dataを捏造.
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define sense-data (stream-map (lambda (x) (sin x)) integers))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

; sense-dataをcdrで1個ずらすだけだと, zero-crossingsの位置がずれるためlast列の最初は0を入れる。
;    ng: 0, 0, -1, 0, 0, 1, 0, 0, 0,
;    ok: 0, 0, 0, -1, 0, 0, 1, 0, 0,
; 加えて, sign-change-detectorは0"から"正負への変化は0になるようにする.


