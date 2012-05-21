;; メッセージパッシング
;;   データの型によって振り分ける"賢明な手続き"の代わりに、
;;   手続き名によって振り分ける"賢明なデータオブジェクト"で仕事をする。


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else 
            (display "error -- Unknown op"))))
  dispatch)

(define (apply-generic op arg) (arg op))


;; TODO: => q2.75, 2.76
