(define (make-monitored f)
  (let ((counter 0))
    (define (how-many-calls?) counter)
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            (else (begin (set! counter (+ counter 1))
                         (f m)))))
    dispatch))

;; まずhow-many-calls?内部手続きをcounterを返すように定義し、
;; dispatch内で、how-many-calls?を()で囲うと手続き呼び出しになってokだった。
