;; 乱数発生器をリセットできると便利である.
;; (rand 'generate) で新しい乱数を生成し、
;; (rand 'reset <new-value>) で内部状態変数を<new-value>にリセットする。

;      良い乱数・悪い乱数
;      http://www001.upp.so-net.ne.jp/isaku/rand.html
;; を参考にCのrand()を真似る. 本当はメルセンヌツイスタを使ったほうが良い

(define random-init 1)
(define (rand-update x)
     (modulo (+ (* x 1103515245) 12345) 2147483647))

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset)
             (lambda (new-x) (set! x new-x)))
            (else (error "unknown request"))))
    dispatch))

; gosh> (rand 'generate)
; 1103527590
; gosh> (rand 'generate)
; 944465040
; gosh> (rand 'generate)
; 1695244727
; gosh> (rand 'generate)
; 1008001095
; gosh> (rand 'generate)
; 235077491
; gosh> ((rand 'reset) 200)
; 200
; gosh> (rand 'generate)
; 1659729351
