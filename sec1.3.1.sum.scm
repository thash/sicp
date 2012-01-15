(add-load-path ".")
(load "my_defs")

; term = procedure that applied to each item
; next = how to jump to the next item (for ex: +1)
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;(print (sum-cubes 1 10))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


;(print (sum-integers 1 10))

; (trace sum) =>...
; CALL sum #[proc] 1 #[proc] 10
;   CALL sum #[proc] 2 #[proc] 10
;     CALL sum #[proc] 3 #[proc] 10
;       CALL sum #[proc] 4 #[proc] 10
;         CALL sum #[proc] 5 #[proc] 10
;         RETN sum 45
;       RETN sum 49
;     RETN sum 52
;   RETN sum 54
; RETN sum 55

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; (print (* 8 (pi-sum 1 100)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; (trace sum)
; (print (integral cube 0 1 0.001))





