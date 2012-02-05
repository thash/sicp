(add-load-path ".")
(load "my_defs")


(define phi (/ (+ 1 (sqrt 5)) 2))
;(print (/ 1 phi))

; my answer
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (iter (+ i 1))))
      ))
  ;(trace iter)
  (iter 1)
  )
; continued fraction

(define (answer n)
  (cont-frac
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  n)
)

(print
(= (round (* 10000 (/ 1 phi)))
   (round (* 10000 (answer 9))))
) ; => #f
(print
(= (round (* 10000 (/ 1 phi)))
   (round (* 10000 (answer 10))))
) ; => #t


(display "--------------------")
(newline)

; (b). iterative process
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i k)
      result
      (iter (+ i 1)
            (/ (n i) (+ (d i) result)))))
  (trace iter)
  (iter 1 0)
  )
; NOTE: incorrect!!
; this procedure should be like this...

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
            (/ (n i) (+ (d i) result)))))
  (trace iter)
  (iter k 0)
  )

(print
  (cont-frac-iter
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  20)
)


