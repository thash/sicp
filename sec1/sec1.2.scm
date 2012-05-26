;; nによって膨らんで行く計算
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; 膨らまない再帰計算
(define (factorial2 n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))


;;(trace (factorial 6))
;;( (factorial2 6))

(time (factorial 2))
