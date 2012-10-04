(load "./stream")

;; stream-limitを定義せよ。ちなみにこんな風に使われる。
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; TODO

