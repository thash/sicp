(load "./sec2.4.2")

; 2.4.3. データ主導プログラミングと加法性
;   前節の実装はふたつの弱点がある。
;     1. 汎用インターフェイス手続き(real-partやmagnitude)は異なる表現の全てを扱わなければならない
;     2. それぞれの表現を別々に設計することが出来るが、システム全体で名前空間が被らないことを保証しなければならない。数百になると厳しい。
;
;  これらの弱点を回避する設計は、"加法的(additive)"である、と言う。

;  データ主導プログラミング(data-directed programming)をはじめる。
;  package、という特別な地位を与えた手続き(実装的にはただの手続き。このへんJSぽい)を用意してやる。

; 話の前提として、put, getという手続きがあると仮定する。仕様は次のようなものである。
;   * (put <op> <type> <item>) => <item>を、"表"の<op> x <type>の所に設定する。
;   * (get <op> <type>) => 取り出す。何も無ければfalseを返す。
; key-value的な対応。op x typeの組をkeyとして表に対応づける。 軸が演算と型のふたつだからできる。
;
; 仮定されてはいるけど、実際に動かすためにtableとget/putを実装する。
;
;   section3.3.3. 表の表現。
;   Q3.25: 任意個数のキーを持つ表を作れ、という問題。
(load "./sec3.3.3")


; rectangularの体系をまとめたpackage
(define (install-rectangular-package)
  ; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) 
    (sqrt (+ (square (real-part z))
              (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ; "システムの他の部分とのインターフェイス" だそうな
  (define (tag x) (attach-tag 'rectangular x)) 
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


; polarの体系をまとめたpackage
(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) 
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  (define (tag x) (attach-tag 'polar x)) 
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


; apply-generic. 演算の名前と引数の型から表を探し、getした手続きを作用させる
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (display "error" ; 本文ではerror手続きを使ってた
                 (list op type-tags))))))


; これを使って、以下のように汎用選択肢を定義できる。
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))


; 実部と虚部があるときはrectangular, 
; 絶対値と偏角があるときはpolarを使う。

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

