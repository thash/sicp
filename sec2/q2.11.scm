(add-load-path ".")
(load "q2.7")

; 図を書いた@EverNote - 2枚目わかりやすく書けた
; 単純にmax/minを取るのでは足りない部分があるからcondで愚直に書き下したよ
(define (mul-interval2 x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y))
        (cond ((> lx 0) ; xは全部正
              (cond ((> ly 0) (make-interval (* lx ly) (* ux uy))) ; yも全部正
                    ((< ly 0) ; yの下端が負で...
                     (cond ((< uy 0) (make-interval (* lx uy) (* ux ly))) ; yの上端も負
                       ((> uy 0) (make-interval (* ux ly) (* ux uy))))))) ; yの上端が正で、yがゼロをまたぐ
              ((< lx 0) ; xの下端が負
               (cond ((> ly 0) ; yは全部正
                      (cond ((> ux 0) (make-interval (* lx uy) (* ux uy))) ; xがゼロをまたぐ。2回の乗算が必要なのはここ。
                            ((< ux 0) (make-interval (* lx uy) (* ux ly))))))) ; xは全部負
              )
        )
    )
  )

; ↑よりも、xとyそれぞれが「ぜんぶ正」「ぜんぶ負」「ゼロをまたぐ」の3通り、3*3=9とした方がきれい。
; elseが「ゼロをまたぐ」やつと決めて。
(define (mul-interval3 x y)
  (cond ((> lx 0) ; xが正
         (cond ((> ly 0) (make-interval (* lx ly) (* ux uy))) ;yも正
               ((< uy 0) (make-interval (* ux ly) (* lx uy))) ;yが負
               ; yがゼロをまたぐ
               (else (make-interval (* ux ly) (* ux uy)))))
        ((< ux 0) ; xが負
         (cond ((> ly 0) (make-interval (* lx uy) (* ux ly))) ;yが正
               ((< uy 0) (make-interval (* ux uy) (* lx ly))) ;yも負
               ; yがゼロをまたぐ
               (else (make-interval (* lx uy) (* lx ly)))))
        (else ; xがゼロをまたぐ
          (cond ((> ly 0) (make-interval (* lx uy) (* ux uy))) ;yが正
                ((< uy 0) (make-interval (* ux ly) (* lx ly))) ;yが負
                ; yもゼロをまたぐ!!!
                (else (make-interval (min (* lx uy) (* ux ly))
                                     (max (* lx ly) (* ux uy))))))
        )
  )


