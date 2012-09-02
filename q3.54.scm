(load "./stream")

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

;;;; p.194の"ストリーム暗黙の定義"、の方法ではうまくいかない。
;; ones     => (1 1 1 1 1 ...)
;(define ones (cons-stream 1 ones))
;; integers => (1 2 3 4 5 ...)
;(define (integers (cons-stream 1 (add-streams ones integers))))
; {{{
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (map stream-car argstreams)!!!
; !!!        At line 43 of "./stream-common.scm"!!!
; !!!  1  (map stream-car argstreams)!!!
; !!!        At line 43 of "./stream-common.scm"!!!
; !!!  2  (add-streams ones integers)!!!
; !!!        At line 13 of "(stdin)"!!!
; !!!  3  proc!!!
; !!!!!!
; !!!  4  (stream-cdr s)!!!
; !!!        At line 51 of "./stream-common.scm"!!!
; !!!!!! (stream-ref factorials 1)
; }}}

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; fibgenを真似たfacgenを作るか、暗黙の定義を使うか
(define factorials
  (cons-stream 1
               (mul-streams factorials integers)))


;;;; 3.5.2冒頭のintegers定義だとうまくいく
;
; gosh> (stream-ref factorials 0)
; 1
; gosh> (stream-ref factorials 2)
; 2
; gosh> (stream-ref factorials 4)
; 24
; gosh> (stream-ref factorials 5)
; 120
; gosh> (stream-ref factorials 6)
; 720

