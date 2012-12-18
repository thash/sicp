;; Benは次の実装が効率良いのでは? と主張した.

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;; 単なる解答だけで言えば効率は良くなる, とのこと.
;; 選ぶ時点で候補を絞っていくので.



