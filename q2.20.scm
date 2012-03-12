; 任意の数の引数を取る手続きの定義
; (define (f x y . z) <body>)
; (f 1 2 3 4 5 6)
; => x=1, y=2, z=(3 4 5 6)
;
; lamndaを使って定義することも出来る
; (define f (lambda (x y . z) <body>))

; "1つかそれを越える個数の整数を取り、先頭と同じ偶奇性を持つ引数のリストを返す手続き"
(define (same-parity x . y)
  (define (true-list proc vals)
    (define (iter vals result)
      (if (null? vals)
        result
        (if (proc (car vals))
          (iter (cdr vals) (append result (list (car vals))))
          (iter (cdr vals) result))))
    (iter vals ()))

  (cond ((even? x) (true-list even? (cons x y)))
        ((odd? x) (true-list odd? (cons x y)))))

; 与えられたprocがtrueのものだけのリストを返す手続きtrue-listを内部で定義



; 引数のリストから偶数だけ返す手続き(↑の部分)
; (define (returneven vals)
;   (define (iter vals result)
;     (if (null? vals)
;       result
;       (if (even? (car vals))
;         (iter (cdr vals) (append result (list (car vals))))
;         ; (cons (car vals) result) だと逆順になる
;         (iter (cdr vals) result))))
;   (iter vals ()))

