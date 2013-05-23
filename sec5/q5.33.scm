(define (factorial n)
  (if (= n 1)
    1
    (* (factorial (- n 1)) n)))

(define (factorial-alt n)
  (if (= n 1)
    1
    (* n (factorial-alt (- n 1))))) ; 引数の順序が違うだけ

;; factorialとfactorial-alt, 効率を比べよ.
;; => push回数は変わらないので効率は同じ.

;; 今回作った積極制御評価器は評価順が右から左なので(要出典), nを先に評価するかfactorialを先に評価するか, が違う.

