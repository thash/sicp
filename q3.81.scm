(load "./sec3.5.5") ;; including stream.scm

;; 代入バージョン: 問題3.6(p.134), q3.6.scm

;; 入力ストリームはこんな感じ. 数値が来たらreset.
;; 'generate 'generate 20 'generate 100 'generate 'generate 'generate ...

;; rand-update, random-initは sec3.5.5.scm より引き継ぎ

(define (random-numbers request-stream)
  ;; generateの時はrand-updateを呼び出し, それ以外の場合は入力数値自身を返す
  (define (dispatch x m)
    (cond ((eq? m 'generate)
           (rand-update x))
          (else m)))
  (cons-stream
    random-init
    (stream-map dispatch (random-numbers request-stream) request-stream)))


(define all-generate (cons-stream 'generate all-generate))

(define req-st
  (cons-stream 'generate
               (cons-stream 'generate
                            (cons-stream 20
                                         (cons-stream 'generate
                                                      (cons-stream 100
                                                                   all-generate))))))

; gosh> (stream-head (random-numbers all-generate) 10)
; 1, 1103527590, 944465040, 1695244727, 1008001095, 235077491, 776026401, 964188548, 19180611, done

; gosh> (stream-head (random-numbers req-st) 10)
; 1, 1103527590, 944465040, 20, 595480775, 100, 829870848, 256704636, 1572076480, done


