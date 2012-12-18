;; flatten-stream が delay を陽に使うのはなぜか.
;; (q4.71と同じじゃね?)
(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream))))))

;; で, delayを使わないversionだと...
(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (flatten-stream (stream-cdr stream)))))
