(add-load-path ".")
(load "my_defs")
(load "sec2.2.3") ; for filter
(load "sec2.2.3-nesting") ; for makr-pair-sum
(load "q1.22") ; for prime?

; 与えられた整数nに対して 1<=j<i<=n の対(i,j)の並びを生成する手続きunique-pairsを定義する。
; また、それを用いてprime-sum-pairsの定義を簡略化する

; まず1..n-1までの整数が全候補。
(define (hoge n)
  (map __proc__ (enumerate-interval 1 (- n 1))))

; __proc__を探す。lambda (j)として入る。取り得るiの値はj+1..nの範囲。
(enumerate-interval (+ j 1) n)

; この範囲それぞれに対して、(i,j)をconsで生成する。
(lambda (j) (map __proc2__ (enumerate-interval (+ j 1) n)))

; jとnは外から与えられている。iが移り変わる。iの取り得る値はj+1がminimumと保証されているので
; proc2 はこのようになる。
(lambda (i) (cons i j))

; まとめるとこんな感じになってくる
(define (hoge n)
  (map (lambda (j)
         (map (lambda (i) (cons i j)) (enumerate-interval (+ j 1) n)))
       (enumerate-interval 1 (- n 1))))

; このままだと結果は
; gosh> (hoge 4)
; (((2 . 1) (3 . 1) (4 . 1)) ((3 . 2) (4 . 2)) ((4 . 3)))
; こうなる。returnするのはlist、と求められている。先に定義したflatmapがまさにこの用途に使えることを思い出す。
; 後に、利用法を統一するためcons -> listに変更する。
(define (unique-pairs n)
  (flatmap (lambda (j)
         (map (lambda (i) (list i j)) (enumerate-interval (+ j 1) n)))
       (enumerate-interval 1 (- n 1))))



; --------------------
; 次に、unique-pairsを用いてprime-sum-pairsを簡単にする。
; unique-pairsでuniqueなpairのlistが取得できるので、次にやるべきはcar pairとcdr pairを足して素数になるかどうか判定するfilter.
; prime? やmake-pair-sumなど過去の演習から持ってくるものが多い。
(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

