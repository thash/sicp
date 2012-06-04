;;問題: 汎用算術演算パッケージに多項式用の=zero?を定義せよ。これはadjoin-termが、その係数自体がまた多項式である多項式に対しても働くものとする。
;;
;; (every =zero? list) everyは、listがすべてzeroかどうかを判定する手続き。
;;                    Rubyで言うEnumerable#allか。
;;
;;
;; 多項式相手にも使えるように。
;; (make-polynomial .. 'x '((5 0) (4 0)))は=zero?が#tだけど'((5 1) (4 0))とすると#fになるような。
;;





