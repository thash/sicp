(load "./sec4.3-nondeterministic")
;; 問題4.51 (q4.51.scm) に述べたようなpermanent-set!と, 問題4.52 (q4.52.scm) のようなif-failを使うと
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             ;;(print "permanent-set!:" pairs) ; debug
             (amb))
           pairs))
;; の評価の結果はどうなるか.

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-set? exp) (analyze-permanent-set! exp)) ;; 追加
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp)) ;; 追加
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-set! exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok
                        (lambda () ;; 失敗継続でold-valueへ戻さない.
                          (fail2))))
             fail))))

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-success exp) (cadr exp))
(define (if-fail-failure exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-fail-success exp)))
        (fproc (analyze (if-fail-failure exp))))
    (lambda (env succeed fail)
      (sproc env
             succeed
             (lambda ()
               (fproc env succeed fail))))))


;;; require定義(driver-loop中) {{{
(define (require p)
  (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
;;; }}} 前提定義

;; prime系補足定義(sec4.3本文, sec1/sec1.2.6.scm より) {{{
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    ;;(print "check...(" a "+" b ")") ; debug
    (require (prime? (+ a b)))
    (list a b)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x) (* x x))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor) ;#?= for test-divisor
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
;; }}}


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        結果       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;; Amb-Eval input:
; (let ((pairs '()))
;   (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
;              (permanent-set! pairs (cons p pairs))
;              (amb))
;            pairs))
;
; ;;; Starting a new problem
;
; ;;; Amb-Eval value
; ((8 35) (3 110) (3 20))
;
; ;;; Amb-Eval input:
; try-again
;
; ;;; There are no more values of
; (let ((pairs '())) (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110)))) (permanent-set! pairs (cons p pairs)) (amb)) pairs))


;;; debug行追加 ;;;
;; http://www.serendip.ws/archives/2578
;; > わかりやすくするために prime-sum-pair と if-fail 第1引数の (amb) の直前に print 文を追加して動作を確認する。

; ;;; Starting a new problem
; check...(1+20)
; check...(1+35)
; check...(1+110)
; check...(3+20)
; permanent-set!:((3 20))
; check...(3+35)
; check...(3+110)
; permanent-set!:((3 110) (3 20))
; check...(5+20)
; check...(5+35)
; check...(5+110)
; check...(8+20)
; check...(8+35)
; permanent-set!:((8 35) (3 110) (3 20))
; check...(8+110)
