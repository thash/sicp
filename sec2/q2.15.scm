;; par1とpar2は, par1よりもpar2の方が「よい」プログラムである. なぜか. ヒントは
;;   > 不確かな数を表現する変数が繰り返し現れないように書けるなら,
;;   > (Alyssaのシステムは) きちんとした誤差限界を返す.
;; というあたりの記述か.

;; 回答: par1では不確かな値(width != 0 なr1,r2のことを指す)を2回ずつ使っており,
;;       ばらつきwidthが2回入るためばらつきが大きくなり得る.

;; 再掲.
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)   ;; r1が1回, r2が1回出現
                (add-interval r1 r2))) ;; r1が1回, r2が1回出現
;; 計, r1, r2ともに2回出現してる.

;; 一方でpar2はそれぞれ1回しか現れない.
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
