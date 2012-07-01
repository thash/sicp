(define cdr-looping?
  (let ((stored '()))
    (lambda (x)
      (cond ((null? x) #f)
            ((memq x stored) #t)
            (else (set! stored (cons x stored))
                  (cdr-looping? (cdr x)))))))


