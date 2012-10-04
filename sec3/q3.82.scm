;; monte-carlo積分 問題3.5(q3.5.scm)をストリームで実装する.
;; 円の面積をmonte-carlo積分で見積もり, そこからpiを計算する.
(load "./sec3.5.5")

;; 問題3.5から random-in-range を持ってくる. integer -> real.
(use srfi-27)
; (define (random x) (random-integer x))
(define (random x) (* (random-real) x))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;; estimate-integralを再実装.
;; proc: 引数を2つ取る
;; ※ x1, x2, y1, y2でrandom-in-rangeを呼ぶのでonesはなんでも良い.
(define (estimate-integral proc x1 x2 y1 y2)
  (stream-map (lambda (m) (* (- x2 x1) (- y2 y1) m))
              (monte-carlo (stream-map proc
                                       (stream-map (lambda (x) (random-in-range x1 x2)) ones)
                                       (stream-map (lambda (x) (random-in-range y1 y2)) ones))
                           0.0 0.0)))

;; 座標(x, y)が単位円の中に入るかどうかのテスト. #t/#fを返す
(define (p-test x y)
  (>= 1 (+ (* x x) (* y y))))


;; #t/#fストリーム列 s を生成
(define s (stream-map p-test
                      (stream-map (lambda (x) (random-in-range -1 1)) ones)
                      (stream-map (lambda (y) (random-in-range -1 1)) ones)))

(define s2 (monte-carlo s 0 0))

; r * r * PI / (x2 - x1) * (y2 - y1)
(define pi
  (stream-map (lambda (p) (* p (* 2 2))) s2))


