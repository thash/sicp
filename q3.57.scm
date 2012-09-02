(load "./stream-without-memo")
;(load "./stream")

;; add-streamsの呼び出し回数を調べる
(define counter 0)
(define (add-streams s1 s2)
  (begin
    (set! counter (+ 1 counter))
    (display "count: ")
    (display counter)
    (newline)
    (stream-map + s1 s2)))

;; from sec3.5.2.scm
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

; (stream-ref fibs 7)
; with-memo    => count:  1
; without-memo => count: 20

; (stream-ref fibs 10)
; with-memo    => count:  1
; without-memo => count: 88

;; 段違い
