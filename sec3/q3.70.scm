(load "./sec3.5.3")
;; 対が"ある有用な順"で現れるようなストリームを定義する。

;; weightは"重み関数" W(i,j). ある基準で対に順序を付ける。
(define (merge-weighted weight s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let* ((s1car (stream-car s1))
             (s1w (weight s1car))
             (s2car (stream-car s2))
             (s2w (weight s2car)))
      (cond ((<= s1w s2w)
             (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
            (else
              (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2)))))))))

;; これを使って、pairsをweightに基づいてmergeしよう。
(define (weighted-pairs weight s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (merge-weighted
      weight
      (stream-map
        (lambda (x) (list (stream-car s1) x))
        (stream-cdr s2))
      (weighted-pairs weight (stream-cdr s1) (stream-cdr s2)))))

;; 陥った問題: 最初の項をweight関係なしにconsしてしまうと不十分。
;;             cons-streamを呼ばないとLouisの二の舞になるので、
;;             最初にdummyのpairを入れておいてcons-stream, とやるとよさげ。

;; (a).
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define sump (weighted-pairs
               (lambda (pair) (apply + pair))
               integers
               integers))

; (display-stream-n sump 20)
; (1 1), (1 2), (1 3), (2 2), (1 4), (2 3), (1 5), (2 4), (3 3), (1 6), (2 5), (3 4), (1 7), (2 6), (3 5), (4 4), (1 8), (2 7), (3 6),

;; (b).
(define no-235-factors (stream-filter
                         (lambda (n) (not (or (divides? 2 n)
                                              (divides? 3 n)
                                              (divides? 5 n))))
                         integers))

(define w-no-235 (weighted-pairs
                   (lambda (pair) (+ (* 2 (car pair))
                                     (* 3 (cadr pair))
                                     (* 5 (car pair) (cadr pair))))
                   no-235-factors
                   no-235-factors))


; (display-stream-n w-no-235 20)
; (1 1)
; (1 7)
; (1 11)
; (1 13)
; (1 17)
; (1 19)
; (1 23)
; (1 29)
; (1 31)
; (7 7)
; (1 37)
; (1 41)
; (1 43)
; (1 47)
; (1 49)
; (1 53)
; (7 11)
; (1 59)
; (1 61)


