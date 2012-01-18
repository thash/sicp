(add-load-path ".")
(load "my_defs")
(load "q1.24")

(newline)
(display "q1.25===========================================================")

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))


;; 簡略版でやると...
;; 一応求まるが、効率が悪い。

1009
10007
100003 ;;遅い

;; squareをして大きい数でmodを出すので時間がかかる、のではないか？x
;; => 実際はmodよりもかけ算じたいが遅い。っぽい。
;; 普通のintに収まらない計算などもあるので。
;; => 実際に書いてみないと。

;; p.29 note 46,
;; x * y modulo m = ((x modulo m) * (y modulo m)) modulo m

;; Bottol neck => ?
;; どこかを調べる。調べるにはどうすればいい？


(define (fermat-test2 n)
  (define (expmod base exp m)
    (remainder (fast-expt base exp) m))

  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (expmod base exp m)
  (define (remainder a b)))

;; 「自分までのかず」から1を弾いてランダムを出してる。テストに時間がかかる理由(...?)
;; 春山さん > 直前のものをみてみる。100002**(100003) つまり10万2の10万3乗をやるとどれくらいかかるか

(fast-expt 100002 100003)

; を計算しよう





