;; > 図5.11に示す階乗計算機を使い, 小さいnのいくつかに対しn!を計算するのに必要な退避の回数とstackの最大深さを計測せよ.
;; > そのデータからn>1でn!を計算するのに使う退避演算の全数と, stackの最大深さのnに関する式を決定せよ.
;; > このそれぞれはnの線形関数で, 2つの定数で決まることに注意しよう.

(define fact-machine
  (make-machine
    '(n val continue)
    (list (list '- -) (list '= =) (list '* *))
    '((assign continue (label fact-done))
      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done
      (perform (op print-stack-statistics))))) ;; 最後に出力を追加


; gosh> (set-register-contents! fact-machine 'n 2)
; gosh> (start fact-machine)
; (total-pushes = 2 maximum-depth = 2)done
;;; この調子でnを変えつつ出力を確認する

2 (total-pushes = 2 maximum-depth = 2)done
3 (total-pushes = 4 maximum-depth = 4)done
4 (total-pushes = 6 maximum-depth = 6)done
5 (total-pushes = 8 maximum-depth = 8)done
6 (total-pushes = 10 maximum-depth = 10)done
7 (total-pushes = 12 maximum-depth = 12)done
8 (total-pushes = 14 maximum-depth = 14)done
12 (total-pushes = 22 maximum-depth = 22)done

;; よっておそらく
total-pushes = maximum-depth = 2 * (n - 1)
