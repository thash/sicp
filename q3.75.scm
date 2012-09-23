;; sense-dataの質が悪くノイズが発生する.
;; 平滑化のためにLouisが実装したプログラムは以下のようなものだが、bugがあるという. つーかLouis(男)助手かよ.

;    (define (make-zero-crossings input-stream last-value)
;      (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;        (cons-stream (sign-change-detector avpt last-value)
;                     (make-zero-crossings (stream-cdr input-stream)
;                                          avpt))))

;; 再帰的に渡しているlast-valueが平均値になっているので生last-valueも渡さないといけないとか?
;; => グラフ描いて見た.
;; https://www.evernote.com/shard/s11/sh/388fd5b5-c2a7-4598-a819-37e1c8ab4af0/d8f7b63cd7e6529f3c2c602572d85394

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

;; 信号処理というかデータ分析の基礎か

