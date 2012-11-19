(load "./sec4.3-nondeterministic")

;(driver-loop)
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


;; http://wqzhang.wordpress.com/2010/04/28/sicp-exercise-4-43/
(define (yacht-puzzle)
; father = (last-name daughter yacht)
  (define (last-name f) (car f))
  (define (daughter f) (cadr f))
  (define (yacht f) (caddr f))
  (define (different? f) (not (eq? (daughter f) (yacht f))))
  (define (father-of d fathers)
    (if (eq? d (daughter (car fathers)))
        (car fathers)
        (father-of d (cdr fathers))))
  (let ((Downing (list 'Downing
                       (amb 'Gabrielle 'Lorna 'Mary 'Rosalind)
                       'Melissa))
        (Hall (list 'Hall
                    (amb 'Gabrielle 'Lorna 'Mary 'Melissa)
                    'Rosalind))
        (Hood (list 'Hood 'Melissa 'Gabrielle))
        (Moore (list 'Moore 'Mary 'Lorna))
        (Parker (list 'Parker
                      (amb 'Gabrielle 'Lorna 'Mary 'Melissa
                           'Rosalind)
                      (amb 'Gabrielle 'Lorna 'Mary 'Melissa
                           'Rosalind))))
    (require (different? Parker))
    (let ((fathers (list Downing Hall Hood Moore Parker)))
      (define (map proc a)
        (if (null? a)
            '()
            (cons (proc (car a)) (map proc (cdr a)))))
      (let ((daughters (map daughter fathers)))
        (require (distinct? daughters))
        (let ((yachts (map yacht fathers)))
          (require (distinct? yachts))
          (require (eq? (daughter Parker)
                        (yacht (father-of 'Gabrielle fathers))))
          fathers)))))


