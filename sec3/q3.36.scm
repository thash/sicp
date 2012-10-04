;; 次の一連の式を大域環境で評価するとしよう:

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

;; set-value!の評価中の辞典で、コネクタ局所手続きの中の次の式が評価された:

(for-each-except setter inform-about-value constraints)

;; 上の式が評価される環境を示す環境図を描け.
;;  => sicp3.36.graffle


