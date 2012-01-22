(add-load-path ".")
(load "my_defs")

(define (identity x) x)
(define (inc n) (+ n 1))


; q1.32(a)
; accumulate - recursive ---------------------------------------------
(display ">>>>>>>>>>>recursive<<<<<<<<<")
(newline)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

; sum
(define (sum term a next b)
  (accumulate + 0 term a next b))

; product
(define (product term a next b)
  (accumulate * 1 term a next b))


; test sum
(define (sum-cubes a b)
  (sum cube a inc b))
(trace accumulate)
(print (sum-cubes 1 10))

; test product
(define (factorial n)
  (product identity 1 inc n))

(trace accumulate)
(print (factorial 5))

; << accumulate - recursive ---------------------------------------------

(newline)
(display "================================================")
(newline)

; q1.32(b)
; accumulate - iterate ---------------------------------------------
(display ">>>>>>>>>>>iterate<<<<<<<<<<<<<<")
(newline)
(define (accumulate2 combiner null-value term a next b)
  (define (iter a stock)
    (if (> a b)
      stock
      (iter (next a) (combiner stock (term a)))))
  (trace iter)
  (iter a null-value))

; sum
(define (sum2 term a next b)
  (accumulate2 + 0 term a next b))

; product
(define (product2 term a next b)
  (accumulate2 * 1 term a next b))

; test sum
(define (sum-cubes a b)
  (sum2 cube a inc b))
(print (sum-cubes 1 10))

; test product
(define (factorial n)
  (product2 identity 1 inc n))

(print (factorial 5))

