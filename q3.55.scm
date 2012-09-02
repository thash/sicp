(load "./stream")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (add-streams s1 s2)
  (stream-map + s1 s2))

; 1 2 3  4  5
;   1 2  3  4
;     1  2  3
;        1  2
;           1
;-------------
; 1 3 6 10 15

(define (partial-sums s)
  (cons-stream
    (stream-car s)
    (add-streams
      (stream-cdr s)
      (partial-sums s))))


