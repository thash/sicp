;; i <= j <= k なる(Si,Ti,Ui)のstreamを生成するtripletsを生成し、
;; それを元にしてピタゴラス数の対ストリームを出せ。
(load "./sec3.5.3")
(load "./my_defs")

(define (triplets s1 s2 s3)
  (cons-stream
    (list
      (stream-car s1)
      (stream-car s2)
      (stream-car s3))
    (interleave
      (stream-map
        (lambda (x) (append (list (stream-car s1)) x))
        (stream-cdr (pairs s2 s3)))
      (triplets
        (stream-cdr s1)
        (stream-cdr s2)
        (stream-cdr s3)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define ppp (triplets integers integers integers))
(define pythagorean (stream-filter
                      (lambda (triplet) (= (square (caddr triplet))
                                           (+ (square (car triplet))
                                              (square (cadr triplet)))))
                      ppp))


; gosh> (display-stream-n ppp 20)
; (1 1 1), (1 1 2), (2 2 2), (1 2 2), (2 2 3), (1 1 3), (3 3 3), (1 2 3), (2 3 3), (1 1 4), (3 3 4), (1 3 3), (2 2 4), (1 1 5), (4 4 4), (1 2 4), (2 3 4), (1 1 6), (3 4 4), done

; gosh> (display-stream-n pythagorean 5)
; (3 4 5), (6 8 10), (5 12 13), (9 12 15),

