;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. レジスタ計算機での計算
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; いままで触れてきたのは既存のLisp実装の上でしかない. もっと深淵に潜る.
;; 伝統的な計算機(レジスタ計算機,レジスタマシン)でのステップごとの操作を使い, プロセスを記述する.
;; レジスタ(= 一組の記憶素子の内容を操作する命令)
;; "要するにアセンブラみたいなのを書きます."

;;;   5.1 レジスタ計算機の設計
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Euclid互徐法による最大公約数の計算
(define (gcd a b)
  (if (= b 0)
    a
    (gcd (remainder a b))))
;; 覚えておくもの
;;   a, b
;; やること
;;   (= b 0)
;;   (remainder a b)

;; => q5.1.scm -- 反復的アルゴリズムを用いたfactorial(階乗)の計算


;;;     5.1.1 レジスタ計算機の記述言語
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 単純な計算なら図を書けばいいが, Lisp処理系のような複雑なものは厳しい.
;; そこでデータパスとcontroller図の与える情報を表現する言語を設計しよう.

;;; データパスとcontrollerで記述する方法 ;;;
;; 未知: data-path, registers, register, source, name, buttons,
;;       operation, operations, inputs, constant
(data-path
  (registers
    ((name a)
     (buttons ((name a<-b) (source (register b))))) ; 図でマルの中にバツがあるものは"button"のつもりらしい
    ((name b)
     (buttons ((name b<-t) (source (register t)))))
    ((name t)
     (buttons ((name t<-r) (source (operation rem))))))

  (operations
    ((name rem)
     (inputs (register a) (register b)))
    ((name =)
     (inputs (register b) (constant 0)))))


;; 未知: controller, test, branch, label, goto
(controller
  test-b ; label
  (test =) ; test -- その結果によってbranchがわかれる.
  (branch (label gcd-done)) ; 条件分岐 -- 文字通りbranch.
  (t<-r)
  (a<-b)
  (b<-t)
  (goto (label test-b)) ; 無条件分岐 - loopしてるのか.
  gcd-done) ; label

;;; ------------------------------- ;;;
;;; controllerだけで完結させると... ;;;
;; 未知: op, reg, assign
(controller
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done)
;; testあたりに注目すると違いがわかる.

;; Pros: 可読性が上がる
;; Cons: 制御式の命令がlisp式っぽく見える

;; => q5.2.scm -- レジスタ記述言語で問題5.1の階乗計算機を記述せよ.


;;; ------------------------------- ;;;
;;; 結果を印字させてみる ;;;
;; 印字命令perform + printは基本演算として使用可能と仮定.
;;   read  どこからともなく入力された値をレジスタに格納
;;   print レジスタの値をここではないどこかへ出力
;; 未知: perform
(controller
  gcd-loop
  (assign a (op read))
  (assign b (op read))
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done
  (perform (op print) (reg a))
  (goto (label gcd-loop)))


;;;     5.1.2 計算機設計における抽象
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; どのレイヤーまで自力実装するかは任意に決められるよーという話.

;;; 図5.6. 改善版GCD計算機のcontroller命令列
(controller
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (reg a))
  rem-loop
  (test (op <) (reg t) (reg b))
  (branch (label rem-done))
  (assign t (op -) (reg t) (reg b))
  (goto (label rem-loop))
  rem-done
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done)

;; => q5.3.scm -- 1.1.7のNewton法で平方根を計算する計算機を設計せよ.


;;;     5.1.3 サブルーチン
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assign命令を拡張して命令列からラベルを値として(特別な定数として)レジスタに代入できるようにする
;; 感想: https://twitter.com/T_Hash/status/287257779998175233

;; 2箇所から使う場合, labelを用意して別々のgcdを書けばいい
;;; 図5.7 二つのGCD計算を持つ計算機のデータパスと制御器命令列の一部 ;;;
gcd-1
(test (op =) (reg b) (const 0))
(branch (label after-gcd-1))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd-1))
after-gcd-1
...
gcd-2
(test (op =) (reg d) (const 0)) ; bではなく reg dを使っている.
(branch (label after-gcd-2))
(assign (s (op rem) (reg c) (reg d))) ; 他のレジスタも名前が違う.
(assign c (reg d))
(assign d (reg s))
(goto (label gcd-2))
after-gcd-2

;; これのレジスタを共有させよう.
;;; 図5.8 二つの異なるGCD計算に同じデータパス部品を使う計算機の制御器命令列の一部 ;;;
gcd-1
(test (op =) (reg b) (const 0))
(branch (label after-gcd-1))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd-1))
after-gcd-1
...
gcd-2
(test (op =) (reg b) (const 0)) ; gcd-1と同じレジスタ名. 以降も.
(branch (label after-gcd-2))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd-2))
after-gcd-2

;; ...けど, 呼び出し元が増えるとすぐ破綻. そこでcontinueを導入.
;; まずはcontinueに0,1,2...という値を入れて区別する想定.
;;; 図5.9 図5.8の重複命令列を避けるためcontinueレジスタを使う ;;;
gcd
(test (op =) (reg b ) (const 0))
(branch (label gcd-done))
(assign t (op rem) (reg a ) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd))
gcd-done
(test (op = ) (reg continue) (const 0)) ; continue "0"のとき
(branch (label after-gcd-1))
(goto (label afterfib-n-2))
...
;; それを必要とする第一の場所から gcdへ分岐する前にcontinue regに0を置く
(assign continue (const 0))
(goto (label gcd))
after-gcd-1
...
;; gcdの第二の使用の前に continue regに1を置く
(assign continue (const 1))
(goto (label gcd))
after-gcd-2

;; でもまあこれ解決になってないよね. gcd使う場所の数だけ数値をインクリメントするのかという
;; => そこでcontinueにlabelを使えるようにする.
;;; 図5.10 continueレジスタにラベルを代入すると図5.9に示した戦略を単純化,一般化できる ;;;
gcd
(test (op =) (reg b) (const 0))
(branch (label gcd-done))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd))
gcd-done
(goto (reg continue))
...
;; gcdを呼び出す前に, gcdから戻るべきラベルをcontinueに代入する
(assign continue (label after-gcd-1))
(goto (label gcd))
after-gcd-1
...
;; gcdへの第二の呼び出しでは継続は異なる.
(assign continue (label after-gcd-2))
(goto (label gcd))
after-gcd-2



;;;     5.1.4 再帰を実装するためのスタックの使用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 階乗も最大公約数も再帰を使う.
;; 先っぽまで降りていったところで実際の演算を行うわけだが,
;; その演算が終わった後に"戻って"きて, 以前のレジスタ内容を復活させる必要がある.
;; それを保存しておくのがLIFOのスタックか！(キューと呼んでも良い？)
;; 再帰的に書けば再帰される, と盲目的に信じていたけどここでは再帰を実現する実装まで降りてきている.
;; いままで見てきた話と5章のレイヤーの違いがよくわかるお話.
;; 反復
;;   layer1 -> layer2 -> ... -> layerN (= answer)
;; 再帰
;;   layer1 -> layer2 ->  ... -> layerN
;;   answer = layer1 <- ... <- layerN
;;
;; ループフェイズは将来計算に使う値をスタックにがんがん詰んで行って,
;; 中央で反転.
;; 計算フェイズではスタックからどんどん取り出してレジスタを更新しまくる

;; 図5.11 再帰的階乗計算機
;; 未知: save, restore
(controller
  (assign continue (label fact-done)) ; 最終帰り番地設定
  fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))
  ;; n と continue を退避し再帰呼び出しを設定する.
  ;; 再帰呼び出しから戻るとき after-fact から
  ;; 計算が続行するように continue を設定
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))
  after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val)) ; valにn(n-1)!がある
  (goto (reg continue)) ; 呼び出し側に戻る
  base-case
  (assign val (const 1)) ; 基底の場合 (1! = 1)
  (goto (reg continue)) ; 呼び出し側に戻る
  fact-done)


;; さらにfibの計算は二重再帰が必要
(controller
  (assign continue (label fib-done))
  fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  ;; Fib(n-1)を計算するよう設定
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n)
  (assign n (op -) (reg n) (const 1))
  (goto (label fib-loop))
  afterfib-n-1
  (restore n)
  (restore continue)
  ;; Fib(n-2)を計算するよう設定
  (assign n (op -) (reg n) (const 2))
  (save continue)
  (assign continue (label afterfib-n-2))
  (save val)                             ; Fib(n-1)をvalとして退避
  (goto (label fib-loop))
  afterfib-n-2
  (assign n (reg val))
  (restore val)
  (restore continue)
  (assign val                        ; Fib(n-1) + Fib(n-2)
          (op +) (reg val) (reg n))
  (goto (reg continue))              ; 呼び出し側に戻る. 答えはvalにある
  immediate-answer
  (assign val (reg n))               ; 基底の場合 Fib(n) = n
  (goto (reg continue))
  fib-done)

;; => q5.4.scm -- レジスタ計算機の規定. controllerとデータパス図を書け.
;; => q5.5.scm -- 階乗とFibonacci計算機を机上シミュレートせよ. スタック途中経過.
;; => q5.6.scm -- Fibonacci計算機から余分なsaveとrestoreを取り除け.


;;;     5.1.5 命令の要約
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ここまできてやっと概要をリストアップしてくれる.

(assign <register-name> (reg <register-name>))
(assign <register-name> (const <register-name>))
(assign <register-name> (op <operation-name>) <input1> ... <inputN>)
(perform (op <operation-name>) <input1> ... <inputN>)
(test (op <operation-name>) <input1> ... <inputN>)
(branch (label <label-name>))
(goto (label <label-name>))

;; ラベルを保持するレジスタと, レジスタ(の中にあるラベルを参照するわけだが)へ飛ぶgoto.
;; まあショートカットだな
(assign <register-name> (label <register-name>))
(goto (reg <register-name>))

;; saveとrestoreでstackを使う
(save <register-name>)
(restore <register-name>)

