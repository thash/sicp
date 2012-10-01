;; monte-carlo積分 (q3.5.scm)をストリームで実装する.
(load "./q3.5")
(load "./sec3.5.5")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define random-init 137)

;; estimate-integralを再実装
(define (estimate-integral p x1 x2 y1 y2)
  (stream-map (lambda (m) (* (- x2 x1) (- y2 y1) m))
              (monte-carlo (stream-map p
                                       (stream-map (lambda (x) (random-in-range x1 x2)) ones)
                                       (stream-map (lambda (x) (random-in-range y1 y2)) ones))
                           0.0 0.0)))

(define (pi-from-monte-carlo-simulation circle-area radius)
  (display circle-area)
  (newline)
  (/ circle-area radius))

(define (p-test x y)
  (<= (+ (square (- x 5)) (square (- y 5))) (square 5)))

;(pi-from-monte-carlo-simulation (display-stream-n (estimate-integral p-test 0 10 0 10) 10) (square 5))
; 100.0, 100.0, 66.66666666666666, 75.0, 80.0, 83.33333333333334, 85.71428571428571, 87.5, 77.77777777777779, done
(pi-from-monte-carlo-simulation (stream-ref (estimate-integral p-test 0 10 0 10) 100000) (square 5))
; 79.02120978790212
