;; lexical-address (A B)
;; A: f-num # frame number
;; B: d-num # displacement number

(define (make-lexical-address f-num d-num) (list f-num d-num))
(define (f-num lexical-address) (car lexical-address))
(define (d-num lexical-address) (cadr lexical-address))

(define (lexical-address-lookup lexical-address ct-env)
  (define (err)
    (error "Unbound address -- LEXICAL-ADDRESS-LOOKUP" lexical-address))
  (define (nth n lst)
    (cond ((null? lst) (err))
          ((= 0 n) (car lst))
          (else (nth (- n 1) (cdr lst)))))
  (let ((frame (nth (f-num lexical-address) env)))
    (let ((val (nth (d-num lexical-address)
                    (frame-values frame))))
      (if (eq? val '*unassigned*) (err)))))

(define (lexical-address-set! lexical-address val env)
  (define (nthcdr n lst)
    (cond ((null? lst)
          (error "Unbound address -- LEXICAL-ADDRESS-SET!" lexical-address))
          ((= 0 n) lst)
      (else (nthcdr (- n 1) (cdr lst)))))
  (let ((frame (car (nthcdr (f-num lexical-address) env))))
    (set-car!
      (nthcdr (d-num lexical-address)
              (frame-values frame))
      val)))

