;; 束縛を除去するunbind! を実装.
;; 仕様が固まってないので適当と思うように決めてね, と.

;; 1. first-frameから除去する. scanでヒットしなければ何もしない.
;; 2. env-loopでframeをさかのぼり, 最初にヒットした束縛を除去する.
;; 3. すべてのframeでヒットした束縛を除去する.

;; 1.はないな. lookup-variable-valueで参照できるのに除去できないのはバランス悪い. というか"評価した環境から与えられた記号の束縛を除去する"という問題文だからそもそも問題外か.
;; env-loopを一度使うだけの温度感が使い勝手良さそうに思える. ので2を選ぶ.

;; そもそもlistからのreject, 的なことができる手続きを考える.
;; 非破壊的で, 取り除いたlistを返す設計.

;; 没
;(define (reject x lis)
;  (let1 original lis
;        (define (inner-reject x inner-list)
;          (if (null? inner-list)
;            '()
;            (if (eq? x (car inner-list)))))))


;; memqを使えるのでは
; (define (reject x lis)
;   (let ((hit (memq x lis)))
;     (if (eq? #f hit)
;       lis
;       (append <scanned-items> (cdr hit)))))
;; scanしたitemを保存しておいて最後にくっつけたいのでmemqは不適当

(define (reject x lis)
  (define (inner-reject x lis scanned)
    (if (null? lis) '()
      (if (eq? x (car lis))
        (append (reverse scanned) (cdr lis))
        (inner-reject x (cdr lis) (cons (car lis) scanned)))))
  (inner-reject x lis '()))

;; unbind!を実装.
(load "./q4.12") ;; 4.12 で抽象化したscan, env-loopを使いたい.
(define (unbind! var env)
  (let ((result (env-loop var env
                          (lambda (vars vals)
                            (set-car! vars (cdr vars))
                            (set-car! vals (cdr vals))
                            #t))))
    (if (eq? #f result)
      (error "Unbound variable -- UNBIND!" var))))

;; http://d.hatena.ne.jp/tmurata/20100318/1268913668
;; このへん見てたら着々とevalに組み込んでる...

