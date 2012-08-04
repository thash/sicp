;;; 『プログラミングGauche』 p.77

(define (append/log . args)
  (print "args=" args)
  (apply append args))

; gosh> (append/log '(a b c) '(1 2 3) '(7 8 9))
; args=((a b c) (1 2 3) (7 8 9))
; (a b c 1 2 3 7 8 9)

; procとして"任意の引数welcome"な手続きを取り、
; 与えられた引数をprintしつつ最終的にはprocと同じ結果を返すmake-logger.

(define (make-logger proc)
  (lambda args
    (print "args=" args)
    (apply func args)))

(define append/log (make-logger append))
(define cons/log (make-logger cons))

