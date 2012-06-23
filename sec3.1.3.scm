;; 3.1.3. 代入を取り入れた対価
;;  set! による局所状態変数にはデメリットもある。
;;  たとえば、1.1.5で説明したような置き換えモデルではもはや説明できなくなる。
;;  代入を使わないプログラムは "関数型プログラミング" という。
;;
;;  代入が話をややこしくしている例を挙げてみる。
;;  資金不足でエラーを出さないwithdrawを考える。

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))

;; gosh> (W 20)
;; 5
;; gosh> (W 10)
;; -5

;; set!を使わずに同じことをやろうとすると...
(define (make-discrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-discrementer 25))

;; gosh> (D 20)
;; 5
;; gosh> (D 10)
;; 15
;; まあ状態保存できないよね。以上の事実を、置き換えモデルの適用で考えてみよう。

;; set!がないと置き換えモデルが使えるんだよ。
;;   ((make-discrementer 25) 20)
;;   -> ((lambda (amount) (- 25 amount))) 20)
;;   -> (lambda (20) (- 25 20)))
;;   -> (- 25 20)
;;   -> 5
;;
;; でもset!があるとそれができないよ。
;;   ((make-simplified-withdraw 25) 20)
;;   -> ((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;;   -> (set! balance (- 25 20)) 25
;;
;;   ここまでこの順出来てしまうと、balanceに5をセットして、でも式の値としては25を返す、と言わなければらない。
;;   しかしset!はそれをさせない。balanceの最初の出現と2回目の出現を区別しなければならず、
;;   これは「記号は値の置き換えである」という置き換えモデルの大前提が覆されていることに起因する。
;;
;;   代入を許すことによって、変数は「名前」から「値が格納される箱」になってしまったのだ。

;; 代入の導入は、「同一」という概念にも影響を及ぼす。
(define D1 (make-discrementer 25))
(define D2 (make-discrementer 25))
;; このD1とD2は同じだが、
(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
;; W1とW2は、それぞれが呼び出された回数によるので、異なる。
;;
;; 式の評価の結果を変えることなく、式の中で「等しいものは等しいもので置き換えられる」という概念の成り立つ言語を
;;   参照透明 (referenctially transparent) であるという。
;; 参照透明性はわれわれの言語にset!を入れたとき破られた。
;; 式を等価な式で置き換えて単純化するタイミングがわからなくなる。そのため代入を行うプログラムは推論が難しい。
;; 参照透明性がなくなった世界では「同じ」であるとは何を意味するのか、形式邸に捉えることが難しくなる。
;; 「同じ」であるかどうかは、「同じ」オブジェクトを2度観測し、2回の観測結果が違っていることが「変化」である。
;; いま「同じ」オブジェクトと言ったが、ここの「同じ」は先験的(a-priori?)な「同一」である。
(define alice-account (make-account 100))
(define bob-account (make-account 100))
;; と
(define alice-account (make-account 100))
(define bob-account alice-account)
;; では違いがある。後者は, bob-accountがalice-accountと「同じ」であると定義した。


;; 関数型プログラミングではない、代入を多用するプログラミングを「命令型プログラミング(imperative programming)」という。命令型に書くと、いらぬバグを入れてしまう可能性が発生する。階乗を計算するプログラムで見てみる。

;; まず再帰的な階乗
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;; 内部の反復ループで引数を渡す代わりに, productとcounterの値を明示的に代入して更新していくとこうなる。
(define (factorial-set n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin (set! product (* counter product))
               (set! counter (+ counter 1))
               (iter))))
    (iter)))

;; ここで仕掛けられた罠は、
; (set! product (* counter product))
; (set! counter (+ counter 1))
;; の順は正しいが、逆の順序で、counter更新後にproductを更新してしまうと正しい答えにならない、というもの。
; (set! counter (+ counter 1))
; (set! product (* counter product))
;
;; 関数型プログラミングにはこの問題 (この代入を後にすべきか先にすべきか問題) は発生しない。


