;; 「θ(logn)をθ(n)にしてしまった」

;; original version
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
; 例: (expmod 2 8 2) = 2^8 % 2
(expmod 2 8 2)
  (expmod 2 4 2)
    (expmod 2 2 2)
      (expmod 2 1 2)
        (expmod 2 0 2)
        1
      0 ; elseの方に入っていて, (expmod...)から1が帰ってきたから 2(base) * 1(返り値) % 2(m) = 0
    0 ; 0 % 2 = 0
  0
0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一方, Louisの書いたプログラム
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;; ココが違う. (square (expmod...)) を (* (expmod...) (expmod...))に変えている.
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

;; original verでは squareの引数として1回評価されていたやつが両方展開されて2回評価してしまっている。

;; 図: http://3.bp.blogspot.com/_PnLYRqe0k9g/S4RADXPrAXI/AAAAAAAAARk/yJESpD2zwhQ/s1600/expmod-mult-diagram.png

;; 結果は同じでもexpmodは再帰的に展開されていくのでコード中に現れる数が
;; 5個 => 15個と激増しているからそら遅くもなるわなと
