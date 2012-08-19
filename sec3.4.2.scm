;; 3.4.2. 並列性の制御機構
;;   並列プロセスの難しいところは、異なるプロセス間で処理が混ざり合うこと.
;;   実用的な並列システムの設計法は、何らかの制限をかけること。一例として
;;     直列変換器(serializer)
;;   を解説する。
;;
;;   "直列化"した一連の手続きは他の手続きとは同時に走り得ない。
;;
;;   直列変換器は引数として手続きを取り、元の手続きと同様に振る舞う直列化した手続きを返す。
;;     使い方:
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

;; これだけのコードであっても、以下の割り込みの可能性が考えられる。
;;
;; 101: P1がxを100にして, 次にP2がxを101にする
;; 121: P2がxを11にして, 次にP1がxを11*11=121にする
;; 110: P1が(* x x)の評価でxの値に2度アクセスする間に, P2がxを10から11に変える
;;  11: P2がxにアクセスし, P1がxを100に設定し, P2がxを11に設定する(P1の働きが消えて無くなる)
;; 100: P1がxに2回アクセスし, P2がxを11に設定し, P1がxを100に設定する(P2の働きが消えて無くなる)

;; そこで次のようにserializerを使って直列化すると、
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))

;; 可能性は101と121のふたつだけが残る。

;; make-account ref: sec3.1.1.scm のmake-accountを直列化したver.
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  ;; ここが違ってる。
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 複数の共有資源を使うとき ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 単純なserializerでは対応できない。

;; 複数の共有資源を使う例
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; Peterがa1とa2を交換し、
;; Paul がa1とa3を交換しようとすると, 口座単位でserializeされてても不整合を生じうる。
;; exchangeの動く全期間にわたってserializeする必要がある。

;; 一つの方法は「両方の口座の直列変換器を使い、全体のexchange手続きを直列化する」こと
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;; ここまで同じ
  (let ((balancer-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balancer-serializer) ;; new!
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))))

;; deposit/withdrawを直列化する。ただし直列化を明示的に管理するのは銀行口座オブジェクトの管理者.
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

