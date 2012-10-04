;; 3.3. 可変データでのモデル化
;;    選択子と構成子に加え、データオブジェクトを変更するための変更子(mutator)を含んだデータ抽象を設計する
;;    変更子 - つまりsetter.
;;    変更子の定義されているオブジェクトを可変データオブジェクト(mutable data objectA)という
;;    本節では, 合成データ作成に使ったcons -- "対"に対してmutatorを定義し, 拡張する.

;; 3.3.1. 可変リスト構造
;;    set-car! とset-cdr! * set!と同じく返り値は重要ではない(実装依存).
;;    (set-car! x y) とすれば、xのcarが示す先をyで置き換える。 * これによってアクセスしなくなったデータ -- garbageが発生する

;;    裏返しに(?), set-car!とset-cdr!を使ってconsを実装することも可能。
(define (get-new-pair) (cons 'a 'b))
(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))


;; 共有と同一 - sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

;; 一般にlistをcons, car, cdrで演算すると共有の検出は不可能。

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(load "./my_defs")
(use gauche.test)
(test-section "eq?")
;; 共有を検出するにはeq?を使う。(eq? x y)はxとyが同じオブジェクトであるかを調べてくれる。
(eqr (eq? (car z1) (cdr z1)) => #t)
(eqr (eq? (car z2) (cdr z2)) => #f)


;; 変数は単なる代入
;;   手続きを使って対を実装。
(define (mycons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- MYCONS" m))))
  dispatch)

;; gosh> (define a (mycons 1 2))
;; gosh> (a 'car)
;; 1

;; 可変データも手続きとして実装できる。
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    ((cond ((eq? m 'car) x)
           ((eq? m 'cdr) y)
           ((eq? m 'set-car!) set-x!)
           ((eq? m 'set-cdr!) set-y!)
           (else (error "Undefined operation -- CONS" m)))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'ser-car!) new-value)
  z)

; make-queueないのでコメントアウト。
; (define q (make-queue))
; (insert-queue! q 'a) ; => a
; (insert-queue! q 'b) ; => a b
; (delete-queue! q)    ; => b
; (insert-queue! q 'c) ; => b c
; (insert-queue! q 'd) ; => b c d
; (delete-queue! q)    ; => c d



