;; letを使って局所変数を明示的に作り出すことも出来る。
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds."))))

;; ここで、letは単に手続き呼び出しのsyntax sugarに過ぎない。 (ref: sec1.3.2)
;;   (let ((<var> <exp>)) <body>)
;; ↓ 次のように置き換えられる
;;   ((lambda (<var>) <body>) <exp>)
;; 実際に置き換えてみる。

(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds.")))
   initial-amount))

;; で、絵をかけと。
;; ref: http://wizardbook.wordpress.com/2010/12/14/exercise-3-10/






