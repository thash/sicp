; list操作を行うmap, append, lengthをaccumulateで再定義する

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; sec2.2で定義したmapはこんなかんじ。
(define (map proc items)
  (if (null? items)
    ()
    (cons (proc (car items))
          (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

; accumulateを使えばこうなる。
(define (map p sequence)
  (accumulate
    (lambda (x y) <xxxx>)
    ()
    sequence))

; lambdaで表されている部分は*だったり+だったりもした。
; どう組み合わせるか. (proc a b)のprocに何を取るか。
; lambda (x y) のyはaccumulateの"処理済み"
(define (map p sequence)
  (accumulate
    (lambda (x y)
      (cons (p x) y))
    ()
    sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1 ))
(append (list 1 2 3) (list 4 5 6))
; 直観的にはseq1 seq2の順で渡せばよさそうだが実際は逆にするのが正しい。展開してみる。
; (append (1 2 3) (4 5 6))
; (acccumulate cons (4 5 6) (1 2 3)) : sequence = (1 2 3)
; (cons 1 (accumulate...    : sequence = (2 3)
; (cons 1 (cons 2 (accumulate...    : sequence = (3)
; (cons 1 (cons 2 (cons 3 (accumulate...    : sequence = () ...if (null? ()) = #t
; (cons 1 (cons 2 (cons 3 (4 5 6))))   : initial = (4 5 6)
; (1 2 3 4 5 6)


; length. この場合内部lambdaのyは数値。
(define (length sequence)
  (accumulate
    (lambda (x y) (+ 1 y))
    0
    sequence))
