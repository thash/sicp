(load "./stream")
;; 3.59で見たようなべき級数の乗算手続きmul-series.
;; streamの要素同士を順々に掛け合わせて1つのstreamを作るのが3.54のmul-streams.
;; streamでできた級数同士を掛けるのがmul-series.

; (define (mul-series s1 s2)
;   (cons-stream <??> (add-streams <??> <??>)))

;; q3.56.scm scale-streamを利用.
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; add-streamsは引数二つしか使えないことに留意.
(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (scale-stream (stream-cdr s2) (stream-car s1))
      (add-streams
        (scale-stream (stream-cdr s1) (stream-car s2))
        (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))))))


;; sin^2 + cos^2 = 1 を確認することでテストできる。
(load "./q3.59")

(define s
  (add-streams
    (mul-series cosine-series cosine-series)
    (mul-series sine-series sine-series)))

; gosh>  (display-stream-n s 10)
; 1, 0, 0, 0, 0, 0, 0, 0, 0, done
;   => Ans: 1.000000000

