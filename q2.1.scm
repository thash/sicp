(add-load-path ".")
(load "my_defs")
(load "sec2.1.1")

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
    (cons (/ (- n) g) (/ (- d) g))
    (cons (/ n g) (/ d g)))))

; (2 . -3)、となるのがNG。この場合は
; (-2 . 3)、にしなければならない。
; gosh> (make-rat 6 -9)
; (-2 . 3)
; ok.
