(add-load-path ".")
(load "my_defs")

; 「aの(n-1)乗」
(power a (- n 1))

; 「nを法として1と合同」
(= (remainder (power a (- n 1)) n) 1)

; 「a<nの少なくとも半分はa^(n-1)を計算するとき、nを法とした1の自明でない平方根が出てくる」

