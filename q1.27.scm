(add-load-path ".")
(load "my_defs")
(load "sec1.2.6.fermat")

; (define (fermat-norandom-test n)
;   (define (try-it a)
;     (= (expmod a n n) a))
;   (try-it ... ))

; さっくり反復が書けない...。

(define (fermat-test2 n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter a)
    (if (= a 1)
        #t
        (and (try-it a)
             (iter (- a 1)))))
  (iter (- n 1)))

; (trace expmod)

(print (fermat-test2 560))

; Carmichael Numbers
(newline)
(display "; Carmichael Numbers")
(print (fermat-test2 561))
(print (fermat-test2 1105))
(print (fermat-test2 1729))
(print (fermat-test2 2465))
(print (fermat-test2 2821))
(print (fermat-test2 6601))
