;; 代入を使わずに
;;

(load "./stream")

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))

(define random-init 137)

(define (random-numbers s-in)
  (define (action x m)
    (cond ((eq? m 'generate)
           (rand-update x))
          (else m)))
  (cons-stream
    random-init
    (stream-map action (random-numbers s-in) s-in)))


(define s0 (cons-stream 'generate s0))
(define s1
  (cons-stream 'generate
               (cons-stream 'generate
                            (cons-stream
                              367
                              (cons-stream
                                'generate (cons-stream random-init s0))))))

(define rs0 (random-numbers s0))
; 137, 3062, 1397, 9182, 1142, 9707, 3692, 4457, 2882,
(define rs1 (random-numbers s1))
; 137, 3062, 1397, 367, 652, 137, 3062, 1397, 9182,

