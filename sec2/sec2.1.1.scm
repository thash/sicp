;; 前段
;; ===============================
;;
;; sec1は単純な数値データのみ扱った. sec2では合成データ(compound data)を扱う.
;; 例: 有理数(分子と分母で表せる数). (分子 . 分母) という対が有理数を表すという決まりを作り, 表現する.

;; +をナマで使うのではなく, データ型に応じて実際の処理を使い分ける抽象的なaddを使うといった話.


;;; まずは実装無視で使い方を考える希望的思考(wishful thinking)により, ratの加減乗除を考える.
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;;; ここから, ratの実際の実装について.
;; まずschemeには対を作るconsというものがありますよと.
(define x (cons 1 2 ))
;; x => (1 . 2)

;; 対の要素を取り出すcdr.
(car x) ;=> 1
(cdr x) ;=> 2

;; この機能を使ってrational(有理数)を作る.
(define  (make-rat n d) (cons n d)) ;; 没版. のちgcd使って改良.
(define (numer x) (car x))
(define (denom x) (cdr x))

;; 例.
(define one-half (make-rat 1 2))

;; 人間にわかりやすい表示. 本当はratにtag付けて判定したほうがいいけど.
(define (print-rat x)
(newline)
(display (numer x))
(display "/")
(display (denom x)))

;(print-rat one-half)

;; better makr-rat.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; => q2.1.scm で正負両方の符号を扱えるverのmake-ratを作るよ
