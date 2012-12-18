(load "./sec4.3-nondeterministic")
(driver-loop)
;;; driver-loopを起動した後に定義する. >>> ココカラ
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;; andでもorでもなく"いずれか一方が正しい" ... xorを作る
(define (xor a b)
  (if a (not b) b))

;; http://wqzhang.wordpress.com/2010/04/27/sicp-exercise-4-42/
(define (liars)
  (define (xor a b)
    (or (and a (not b)) (and (not a) b)))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;; (´；ω；｀)
; ;;; Amb-Eval input:
; (liars)
; ;;; Starting a new problem
; !!!*** ERROR: invalid application: (#<syntax and> #f #t)!!!
; !!!Stack Trace:!!!
; !!!_______________________________________!!!
; !!!  0  (apply-primitive-procedure proc args)!!!
; !!!        At line 497 of "./sec4.3-nondeterministic.scm"!!!
; !!!!!!gosh>


