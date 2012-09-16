(load "./sec3.5.3")
;; i <= j の条件を外したすべての(i,j)対ストリームを生成するようにせよ。
;; pairsの定義と比較。
(define (all-pairs s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (interleave
      (stream-map
        (lambda (x) (list (stream-car s1) x))
        (stream-cdr s2))
      (interleave
        (stream-map
          (lambda (x) (list x (stream-car s2)))
          (stream-cdr s1))
        (all-pairs (stream-cdr s1) (stream-cdr s2))))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define s (all-pairs integers integers))

; (display-stream-n s 20)
; (1 1), (1 2), (2 1), (1 3), (2 2), (1 4), (3 1), (1 5), (2 3), (1 6), (4 1), (1 7), (3 2), (1 8), (5 1), (1 9), (2 4), (1 10), (6 1),
