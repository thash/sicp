;; sec2.5.3.scm で多項式を表現するために係数をリストで表現したことを応用して、べき級数の係数を要素とするストリームを考える。
(load "./stream")

;; (a). まずは係数だけに着目してべき級数の積分.
;; 級数
;;   a_0 + a_1*x + a_2*x^2 + a_3*x^3 + ...
;; の積分は
;;   C + a_0*x + (1/2)*a_1*x^2 + (1/3)*a_2+a_3*x^3 + ...
;; である。
;; 定数校Cは除いた係数列を返すintegrate-series。

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series stream)
  (stream-map / stream integers))

; gosh> (display-stream-n (integrate-series ones) 10)
; 1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, done
; gosh> (display-stream-n (integrate-series integers) 10)
; 1, 1, 1, 1, 1, 1, 1, 1, 1, done


;; (b). e^xの微分は自分自身であることを利用して、cosとsinの級数を生成する.

; exp-seriesはまあonesだよね
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; sin' =  cos
;; cos' = -sin

;; e^iθ = cosθ + i*sinθ (オイラーの公式)
;;   =>
;; sinθ = (e^iθ - e^(-iθ))/2
;; cosθ = (e^iθ + e^(-iθ))/2


(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


