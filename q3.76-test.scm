(load "./my_defs")
(prepare-test)

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define sense-data (stream-map (lambda (x) (sin x)) integers))

;(display-stream-n sense-data 5)
;(newline)
;(display-stream-n (smooth sense-data) 5)
;(newline)
;(newline)

(test-section "smooth")
(eqr (stream-ref (smooth sense-data) 0) => (/ (+ (stream-ref sense-data 0) (stream-ref sense-data 1)) 2))
(eqr (stream-ref (smooth sense-data) 1) => (/ (+ (stream-ref sense-data 1) (stream-ref sense-data 2)) 2))
(eqr (stream-ref (smooth sense-data) 20) => (/ (+ (stream-ref sense-data 20) (stream-ref sense-data 21)) 2))


