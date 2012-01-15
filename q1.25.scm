(add-load-path ".")
(load "my_defs")
(load "q1.24")

(newline)
(display "q1.25===========================================================")

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

