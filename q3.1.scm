(define (make-accumulator sum)
  (lambda (i)
    (set! sum (+ i sum))))
