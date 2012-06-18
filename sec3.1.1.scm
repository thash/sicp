;; 3. 標準部品化, オブジェクトおよび状態
;; 3.1.1. 局所状態変数

;; 口座から引き落とすモデルNo.1. この設計にはbalanceがどこからでもアクセスできるという問題がある。
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds."))

;; set! という見慣れない手続きが。 見たとおり、(set a b)はaの値をb式評価結果で置き換える。破壊的だと!付き、というのはRubyと同じ。
;; また、begin も初見。 与えられた引数を順に評価し、最後の引数をbegin全体の返り値とする。これはわりと便利。
;; 勉強会情報: set! が何を返すべきか、という点は決められておらず、処理系依存だそうな。

;; モデルNo.2. 「balanceがどこからでもアクセスできる」という問題を解決した形。
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds."))))

;; 3.2でいろいろ改良されうるが、ひとまず一連の手続きを定義してみよう。
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds.")))

;; make-withdrawを使って、独立な口座が作れる。
; (define W1 (make-withdraw 100))
; (define W2 (make-withdraw 100))
; gosh> W1
; #<closure (make-withdraw make-withdraw)>
; gosh> (W1 20)
; 80
;
; キモは、make-withdrawを使って作成したオブジェクト(?)が手続きとして呼び出し可能であること。

;; 引出の他に、預入(deposit)機能も持つaccountオブジェクトを作る。
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; make-accountは次のように使う。
; gosh> (define acc (make-account 100))
; gosh> acc
; #<closure (make-account dispatch)>
; gosh> ((acc 'withdraw) 60)
; 40
; gosh> ((acc 'deposit) 200)
; 240
; gosh> ((acc 'withdraw) 3000)
; "Insufficient funds."
;
; OOPっぽいね！

