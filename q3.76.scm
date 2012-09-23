;; 平滑化部分をmake-zero-crossingsから分離させる.
(load "./sec3.5.3")

; ボツ. map版のほうがよい
; (define (smooth stream)
;   (if (not (stream-null? (stream-cdr stream)))
;     (let((avpt (/ (+ (stream-car stream) (stream-car (stream-cdr stream))) 2)))
;       (cons-stream avpt
;                    (smooth (stream-cdr stream))))))

(define (smooth stream)
  (stream-map
    (lambda (x1 x2) (/ (+ x1 x2) 2))
    stream
    (stream-cdr stream)))

;; q3.74.scm のstream-map版と組み合わせよう.
(load "./q3.74")
(define zero-crossings (stream-map sign-change-detector (smooth sense-data)
                                                        (cons-stream 0 (smooth sense-data))))

(display-stream-n sense-data 10)
(newline)
(display-stream-n (smooth sense-data) 10)
(newline)
(display-stream-n zero-crossings 10)

; 0.8414709848078965, 0.9092974268256817, 0.1411200080598672, -0.7568024953079282, -0.9589242746631385, -0.27941549819892586, 0.6569865987187891, 0.9893582466233818, 0.4121184852417566,
; 0.8753842058167891, 0.5252087174427744, -0.3078412436240305, -0.8578633849855333, -0.6191698864310322, 0.1887855502599316, 0.8231724226710855, 0.7007383659325692, -0.06595131282380665,
; 0, 0, -1, 0, 0, 1, 0, 0, -1,
