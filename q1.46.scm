(add-load-path ".")
(load "my_defs")
(load "sec1.3.3") ; for fixed-point

; 予測値の改良を繰り返す反復改良法(iterative improvement)という計算戦略。

; from http://wiki.drewhess.com/wiki/SICP_exercise_1.46
(define (iterative-improve good-enough? improve)
  (define (check guess)
    (let ((next-guess (improve guess)))
      (if (good-enough? guess next-guess)
          next-guess
          (check next-guess))))
  (lambda (initial-guess)
    (check initial-guess)))

(define (sqrt x)
  (define (good-enough? guess next-guess)
    (< (abs (- (square next-guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))

(exact->inexact (sqrt 9))

