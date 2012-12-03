;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.4. 論理的プログラミング

;;; 前口上 {{{2
;; 計算機科学 | 数学
;; -------------------
;; 命令的     | 宣言的
;; how        | what

;; 式主導(Expression-oriented)の言語に関する"しゃれ(pun)"
;; "関数の値を記述する式は, またその値を計算する方法として解釈される"
;; (an expression that describe the value of a function may also be interpreted as a means of computing that value.)

;; 4.3節の非決定性プログラミングでは, 式は1つ以上の値を取ることができ, 計算は一価関数というよりは"関係"の記述になる.
;;  => 論理的プログラミングは4.3の拡張であり,
;;     関係的視点とユニフィケーション(unification)という記号パターンマッチングを組み合わせたものである.

;; 論理的プログラミングの力の由来は, 単一のwhatが多くのhow要素を持つことである.

;; 例: append
;    (define (append x y)
;      (if (null? x)
;        y
;        (cons (car x) (append (cdr x) y))))
;; この手続は,
;;   1. 第一のリストがカラの場合
;;   2. ふたつの部分のcons
;; というふたつの規則を"Lispに翻訳"したものと考えられる.
;; これらの規則によって
;; append('(a b) '(c d)) => ? という問題を解くだけではなく
;; append(x y) => '(a b c d) となるすべてのx,yを見つけよ
;; といった問題に答えることもできる.
;;
;; 条件を満たす
;; }}}2

;; 評価器の実装
;; 実装は4.4.4. から始まるが, それまでのコードも動かしたいよねってことで動くやつを持ってくる.
;; ref: http://www.serendip.ws/archives/2617
;; stream-common.scmと, 2次元table処理だけ切り出したtable.scmをloadすれば動いた.

(load "./sec4.4-Serendip")
;(query-driver-loop)

;;; 4.4.1. 推論的情報検索 サンプルデータ
;; query-driver-loopを回した後に以下を実行する.
;; (supervisor A B) は, BがAを監視する.
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))
(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))
(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))
(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer) (computer programmer trainee)))
(assert! (can-do-job (administration secretary) (administration big wheel))) ;; 秘書は社長に取って代われる. ﾜﾛﾀ

;;; Queryの出し方 ;;;
;; リストの数と内容完全マッチの時
;  => (job ?x (computer programmer))
;
;; リストの数を指定, 内容1個マッチの時
;  => (job ?x (computer ?type))
;
;; リストの数は任意, 内容1個マッチの時
;  => (job ?x (computer . ?type))

;; assert!で放り込んだデータがそのまま型にハマるものを探してくる感じ.
;; ?のところは任意としてる.
;; => q4.55.scm - q4.63.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 規則(rule) ;;;
(assert! (rule (outranked-by ?staff-person ?boss)
               (or (supervisor ?staff-person ?boss)
                   (and (supervisor ?staff-person ?middle-manager)
                        (outranked-by ?middle-manager ?boss))))) ;; 再帰的に定義


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; プログラムとしての論理 ;;;
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))


;;; Query input:
(append-to-form (a b) (c d) ?z)
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form (a b) ?y (a b c d))
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input: くっつけると(a b c d)になるペアをすべて.
(append-to-form ?x ?y (a b c d))
;;; Query results:
(append-to-form (a b c d) () (a b c d))
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))

