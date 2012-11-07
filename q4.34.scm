;; 評価器のdriver-loopを修正し, 遅延対とリストが正当に(?)印字できるようにせよ.
;; (無限リストに対してどうするか)
;; 評価器がそれを印字するために認識できるよう, 遅延対の表現も修正する必要がある.

;; http://wqzhang.wordpress.com/2010/04/21/sicp-exercise-4-34/ の写経を行う

;; NEW!
; list-lambda?
; make-list-procedure
; normal-procedure?
; list-procedure?
; (var) print-elements-max
; list-proc->list
;   (inner) apply-proc-to-list
;   (inner) list-element
;   (inner) make-it-normal

(define (eval exp env)
  ;...
  ((list-lambda? exp)
   (make-list-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env))
  ;...
  )

(define (list-lambda? exp) (tagged-list? exp 'list-lambda))
(define (make-list-procedure parameters body env)
  (list 'list-procedure parameters body env))

(define (compound-procedure? p)
  (or (normal-procedure? p)
      (list-procedure? p)))

(define (normal-procedure? p)
  (tagged-list? p 'procedure))
(define (list-procedure? p)
  (tagged-list? p 'list-procedure))

(define print-elements-max 5)

(define (user-print object)
  (cond ((normal-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((list-procedure? object)
         (display (list-proc->list object print-elements-max)))
        (else
          (display object))))


(define (list-proc->list list-proc count)
  (define (apply-proc-to-list proc lst env)
    (eval-sequence
      (procedure-body proc)
      (extend-environment
        (procedure-parameters proc)
        lst
        (procedure-environment proc))))
  (define (list-element option)
    (force-it
      (apply-proc-to-list (actual-value option the-global-environment)
                          (list list-proc) the-global-environment)))
  (define (make-it-normal x n)
    (if (list-procedure? x)
      (if (eq? n 0)
        '(......)
        (list-proc->list x n))
      x))
  ;; 本体
  (cons (make-it-normal (list-element 'car) print-elements-max)
        (make-it-normal (list-element 'cdr) (- count 1)))))






