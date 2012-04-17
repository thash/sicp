(add-load-path ".")
(load "my_defs")
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))
(trace f)
(f 5)

;;;;;;;;;;;;

(define (iter a b c count)
  (cond ((= count 0) c)
        ((= count 1) b)
        (else (iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))

(define (f-iter n) (iter 2 1 0 n))

(trace iter)
(f-iter 5)
