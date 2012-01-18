(add-load-path ".")
(load "my_defs")
(load "sec1.3.1.sum")

; QUESTION
; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        ;(iter (next a) (term a)))) ; my answer
        (iter (next a) (+ result (term a)))))
  (iter a 0))


