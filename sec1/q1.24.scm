(add-load-path ".")
(load "my_defs")
(load "sec1.2.6.fermat")
(load "q1.22")
(require 'random)

(newline)
(display "===========================================================")

(define (timed-prime-test3 n times)
  (newline)
  (display n)
  (start-prime-test3 n times (runtime)))

(define (start-prime-test3 n times start-time)
  (if (fast-prime? n times)
    (report-prime (- (runtime) start-time))))

;(trace expmod)

(timed-prime-test3 1009 100)
; (timed-prime-test3 1013 100)
; (timed-prime-test3 1019 100)
;
; (timed-prime-test3 10007 100)
; (timed-prime-test3 10009 100)
; (timed-prime-test3 10037 100)
;
; (timed-prime-test3 100003 100)
; (timed-prime-test3 100019 100)
; (timed-prime-test3 100043 100)
;
; (timed-prime-test3 1000003 100)
; (timed-prime-test3 1000033 100)
; (timed-prime-test3 1000037 100)
;
