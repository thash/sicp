;; (define (count-pairs x)
;;   (let ((stored '()))
;;     (cond ((not (pair? x)) 0)
;;           ((memq x stored) 0)
;;           (else
;;             (begin (display stored)
;;               (set! stored (cons x stored))
;;                    (+ (count-pairs (car x))
;;                       (count-pairs (cdr x))
;;                       1))))))

;; これだと毎回storedを()に再定義してしまい、状態を引き継げていない. うーん.
;; ↓
;; lambda形式に書き換えたら通った. let定義をlambda本体から外せばいいのか.

(define count-pairs
  (let ((stored '()))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x stored) 0)
            (else
              (begin (newline) (display stored)
                     (set! stored (cons x stored))
                     (+ (count-pairs (car x))
                        (count-pairs (cdr x))
                        1)))))))

