(add-load-path ".")
(load "my_defs")

; from http://www.serendip.ws/archives/431
(define phi (/ (+ 1 (sqrt 5)) 2))

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i ) (+ (d i) (iter (+ i 1))))))
;  (trace iter)
  (iter 1))


(define (iter-a-to-b f a b)
  (newline)
  (display a)
  (display "--> ")
  (if (> a b)
    (f a)
    (and (display (f a)) (iter-a-to-b f (+ a 1) b))))

; (iter-a-to-b
;   (lambda (k)
;     (cont-frac (lambda (i) 1.0)
;                (lambda (i) 1.0)
;                k))
;   1 20)

