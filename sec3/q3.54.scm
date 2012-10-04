(load "./stream")

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

;;;; p.194の"ストリーム暗黙の定義"、の方法ではうまくいかない。
;;;; 3.5.2冒頭のintegers定義だとうまくいく
;;; => そんなことない。単なるtypo

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; fibgenを真似たfacgenを作るか、暗黙の定義を使うか
(define factorials
  (cons-stream 1
               (mul-streams factorials integers)))


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

