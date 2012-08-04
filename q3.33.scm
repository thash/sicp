(load "./sec3.3.5")

;; 愚直にadderをマネて作ったaverager
(define (averager-prototype a b c)

  (define (process-new-value)
    (cond ((and (has-value? a) (has-value? b))
           (set-value! c
                       (/ (+ (get-value a) (get-value b)) 2)
                       me))
          ((and (has-value? a) (has-value? c))
           (set-value! b
                       (- (* 2 (get-value c)) (get-value a))
                       me))
          ((and (has-value? b) (has-value? c))
           (set-value! a
                       (- (* 2 (get-value c)) (get-value b))
                       me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value))

  ;; Gauche組み込みのmatchを使った
  (define (me request)
    (match request
      ('I-have-a-value (process-new-value))
      ('I-lost-my-value (process-forget-value))
      (else (error "Unknown request -- AVERATER" request))))

  (connect a me)
  (connect b me)
  (connect c me)
  me)

; こんなことしなくても、問題文にあるとおりadder制約とmultiplier制約を使えばよい。
; 例の celsius-fahrenheit-converter でやっていることと同じ。
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier c y x)
    (constant 2 y)))

