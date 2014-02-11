(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))

(define-macro (delay proc)
  `(memo-proc (lambda () ,proc)))

(add-load-path "." :relative)
(load "stream-common")
