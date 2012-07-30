;; http://d.hatena.ne.jp/tmurata/20100122/1264117816

(define (ripple-carry-adder ak bk sk c)
  (if (not (null? ak))
    (let ((c-out (make-wire)))
      (full-adder (car ak) (car bk) c (car sk) c-out)
      (ripple-carry-adder (cdr ak) (cdr bk) (cdr sk) c-out))
    'ok))

;; 回路, Full/Half adderを構成要素(function boxes)まで分解して、

