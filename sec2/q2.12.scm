;; q2.11まで作っていたシステムは'幅'を持つが,
;; 実際の世界では3.0 ± 0.15 という誤差を扱いたい. というわけで改良する.
;; まえに作ったwidthがここでは0.15にあたる.

;; 中央値とパーセント相対許容誤差をとり, 望みどおりの区間を返す構成子make-center-percentを定義せよ.
;; また区間のパーセント相対許容誤差を返す選択子percentを定義しなければならない. center選択子は上に示したのと同じで良い.

(add-load-path ".")
(add-load-path "./sec2")
(load "q2.9") ;; 2.9でwidthを定義してる
(load "my_defs")

;; まずAlyssaは3.0 ± 0.15 を表現するために3.0, 0.15のふたつを引数にとる
;; make-center-widthを定義した.
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; が、パーセント許容誤差で定義したいそうだ.
;; 表記を決定するが, 今回は20%は0.2ではなく20と表すことにする。
;; 3.0 ± 0.15 を表現するために渡す引数は 3.0, 5 となる.

;; これから作る手続き間の関係は以下のとおり.
;; center(value), width(value) -- make-center-width(proc) --> interval
;; center(value), percent(value) -- make-center-percent(proc) --> interval
;; interval -- center(proc), percent(proc) --> width(value), center(value)

; 中央値とパーセント相対許容誤差をとり、区間を返すmake-center-percentを定義する
(define (make-center-percent1 c p)
  (make-interval (- c (* c (/ p 100)))
                 (+ c (* c (/ p 100)))))

; ちょっと簡単にこれでもいけるはず

(define (make-center-percent2 c p)
  (make-interval (* c (- 1.0 (/ p 100)))
                 (* c (+ 1.0 (/ p 100)))))

;; 次は逆方向.
;; 区間が与えられたら, 誤差は何パーセントか返す. 5%は0.05ではなく5と表す方針.
(define (percent interval)
  (* 100 (/ (width interval) (center interval))))


(test-section "q2.12")
(define (round2 x)
  (/ (round (* 100 x)) 100))
(eqr (round2 0.1357) => 0.14)

(define i0 (make-interval 2.85 3.15))
(eqr (round2 (exact->inexact (width i0))) => 0.15)
(eqr (round2 (exact->inexact (percent i0))) => 5.0)

(define (make-center-percent c p)
  (let ((wid (* c (/ p 100))))
    (make-interval (- c wid) (+ c wid))))

(eqr (make-center-percent 3.0 5) => (make-interval 2.85 3.15))

