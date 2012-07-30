;; 論理和は論理積と否定を使って表現できる。Terminal Vimで論理記号が書けないので説明し辛い
;; http://d.hatena.ne.jp/tmurata/20100121/1264045686

(load "./sec3.3.4")

(define (or-gate a1 a2 output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a1 c)
    (inverter a2 d)
    (and-gate c d e)
    (inverter e output)
    'ok))

;; and-gate-delayとinverter-delayを使ってこのor-gateの遅延を表現すると、
;; inverter-delay + and-gate-delay + inverter-delay, となる。(最初のinverter-delayは並列で走る)

