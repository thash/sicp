;; n x n board に n個のqueenを置く.

; n = 1 ... そもそも問題が成立しない
;    |x|
; n = 2 ... どうあがいても絶望
;    |x  |   |x  |   |x x|
;    |  x|,  |x  |,  |   |
; n = 3 ... これも解がない.
;    |x    |  |x    |
;    |    x|  |    x|
;    |  x  |, |  x  |, ...
; そもそも周囲8マスはテリトリーなので, 3x3に3個置こうとするとそれだけで詰む
; 初めて解が現れるのがn = 4だそうな.
; n = 4 ... 答えが何個か知らないが, とりあえず以下のを見つけた.
;    |  x    |
;    |      x|
;    |x      |
;    |    x  |


;; SICP本文の解き方をなぞるにあたって, "前提"を把握しておくべき.
;;
;;   1. 列(Column)単位でQueenの置き場所を決める
;;        同じ列に2個queen置いた時点でアウトなのでこの前提は理にかなっているが, 恣意的である.
;;        すべての可能性からスタートするのであれば4 x 4の行列を用意すべきであるが, そうしていない.
;;        つまりすべての可能性は列挙せず, 表現しやすい前提をおいて最初から"絞って"いる.
;;
;;   2. 置き場所を列ごとに並べたlistが"queenの配置"を表している.
;;         listの1要素目は左端1列目, そして値は行(row)のindexを表現する.
;;         例えば4 x 4 のboardにおいて (3 2 1 4) は以下の配置を示す. この配置自体は解じゃないけど.
;          |    x  |
;          |  x    |
;          |x      |
;          |      x|


;; ここまでを背景知識として問題文のコードを見るとなんとなく掴めてくる
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 解読
(if (= k 0) (list empty-board)) ;; は再帰の終了条件.

;; このfilter, lambda内でsafe?を使っていることから実質的な可否判定を行なっている.
(filter (lambda...) (flatmap...))
;; flatmapの返り値は (lambda (positions) (safe? k positions)) のpositionsに"1個ずつ"渡されることになる.
;; こんな感じだろうか? あまり自信はない.
;;   * board-size = 4, k = 4: (safe? 4 ())...
;;       * flatmap返り値は(())
;;   * board-size = 4, k = 3: (safe? 3 (2))...
;;       * flatmap返り値は((1) (2) (3) (4))
;;   * board-size = 4, k = 2: (safe? 2 (4 2))...
;;       * flatmap返り値は((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) (2 3)...)
;;   * board-size = 4, k = 1: (safe? 1 (3 4 2))...
;;       * flatmap返り値は((1 1 1) (1 2 1) (1 3 1) (1 4 1) (2 1 1) (2 2 1)...)
;;   * board-size = 4, k = 0: '()
;; safe?に合格したすべてをappend.

;; flatmapの定義は以下の通り
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
;; appendしたlistを返す. その際seqの各要素にprocを作用させる.
;; eight queenの例で見ると...
(flatmap
  (lambda (rest-of-queens) ;; ここがproc
    (map (lambda (new-row)
           (adjoin-position new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
  (queen-cols (- k 1))) ;; 対象となるseq
;; このflatmap自体がqueen-cols手続きの内部にあるので最後の(queen-cols (- k 1))は再帰.
;; enumerate-intervalは見ての通りの代物で 1からboard-sizeまでの数値リストを返す.
; gosh> (enumerate-interval 1 8)
; (1 2 3 4 5 6 7 8)
; 問題文読み直して気づいたけどここは"行"の候補か. 列は文脈で固定なので.

;; で,
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
;; それをmapしてる...mapに渡す手続きは(adjoin-position new-row k rest-of-queens)とのこと.
;; ここでnew-rowには1からboard-sizeまでの数値が順々に入ってくる.

;; adjoin-positionの定義(いちおうこれが回答になる)
(define (adjoin-position new-row column rest-of-queens)
  (cons new-row rest-of-queens))
;; column (ここではk) を無視してnew-rowとrest-of-queensをconsしてる.
;; new-rowにはenumerate-intervalした数値が1からboard-sizeまで1個ずつ入ってくるのだから,
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
;; の結果は以下のようになる. NOTE:末尾じゃなく頭にcons.
;; ((1 . rest-of-queens) (2 . rest-of-queens) (3 . rest-of-queens)...)
;; そしてrest-of-queensの中身には
(flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
  (queen-cols (- k 1)))
;; のコードから, queen-colsの結果listが1個ずつ(flatmapなので)渡される.
;; ここでようやく再帰終了間際から実際の値を入れて見ることができる.
;; queen-colsがk=1で呼び出されたと考える.
  (define (queen-cols 1)
    (if (= 1 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? 1 positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row 1 rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- 1 1))))))
;; 末尾に注目. (queen-cols 0) は (list '()) => (()) となる
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row 1 rest-of-queens))
                 (enumerate-interval 1 board-size)))
          '(()))
;; rest-of-queensに()が入る. あ, board-sizeは4とでもしとこう.
            (map (lambda (new-row)
                   (adjoin-position new-row 1 '()))
                 (1 2 3 4))
;; mapを実行するとこうなる.
((1) (2) (3) (4))
;; あとは
(filter (lambda (positions) (safe? 1 positions))
        ((1) (2) (3) (4)))
;; ここまで来るとsafe?の実装を見ればよい.

;; 内部手続きnext-column-safe?を再帰的に呼び出している.
(define (safe? column positions)
  (define (next-column-safe? new-row positions row-offset)
    (if (null? positions)
      #t
      (let ((this-row (car positions)))
        (if (or (= this-row new-row) ; b
                (= (+ this-row row-offset) new-row) ; c
                (= (- this-row row-offset) new-row)) ; a
          #f
          (next-column-safe? new-row (cdr positions) (+ 1 row-offset))))))
  (next-column-safe? (car positions) (cdr positions) 1))

;; 再帰の部分, positionsは候補を消費しているのでいいとして,
;; row-offsetをcount upしながら呼び出しているのがキモ.
(next-column-safe? new-row (cdr positions) (+ 1 row-offset))
;    |       |
;    |  a    |
;    |x b    |
;    |  c    |
;; xがsafe?かどうかチェックするとき, まずは隣が0,-1,+1に当たらないか調べるだろ.
;; その次の行を調べるとき, 下図のように0, -2, +2(場外)を見ないといけない.
;    |    d  |
;    |       |
;    |x   e  |
;    |       |
;; 遠ざかる分ナナメ補正しているのがrow-offset.
;; で, 最後までいずれかに合致して#fを返すことなくpositionsをnullにできたらめでたく#t = safe.
