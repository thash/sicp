;; letの亜種その2, 名前付きlet.
(let <var> <bindings> <body>)

;; > <bindings> と<body>は通常のletと同じであるが, 違いは<var>が<body>の中で, 本体を<body>とし, パラメタを<bindings>の変数とする手続きに束縛されることである. そこで<var>と名前のついた手続きを呼ぶことで, <body>を何度でも繰り返し実行することができる.
;; なるほど, わからん.
;; => あー, これ要するに中でdefineしてるのと同じやろ.

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))

;; q4.6.scm のlet->combination を, この名前付きletが使えるように修正せよ, とな.

(define (named-let? exp)
  (if (variable? (cadr exp)) #t #f))

(define (named-let-name exp) (cadr exp))
(define (named-let-assignment exp) (caddr exp))
(define (named-let-body exp) (cdr (cdr (cdr exp))))

(define (transform-named-let name assignment body)
  (sequence->exp
    (list (cons 'define
                (cons (cons name (let-var assignment))
                      body))
          (cons name (let-exp assignment)))))

(define (let->combination exp)
  (if (named-let? exp)
    (transform-named-let (named-let-name exp)
                         (named-let-assignment exp)
                         (named-let-body exp))
    (transform-let (let-assignment exp) (let-body exp))))


