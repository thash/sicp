(add-load-path ".")
(load "my_defs")

(define (f g)
  (g 2))

; f and g are both procedure (?)
; fの引数として渡した手続き(g)を2に作用させて返す。

(print (f square))
(print (f (lambda (z) (* z (+ z 1)))))

(trace f)

; question.
(print (f f))
; => gosh: "error": invalid application: (2 2)

; (f f) ; given procedure is "f"
; (f (g 2))
; (f (f 2)) ; given procedure is "2"
; (f (2 2)) => error

