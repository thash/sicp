(load "./sec4.4-Serendip")
(query-driver-loop)

;; programmerの給与合計を知るにはこうする.
(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))

;; Benは抽象化してfold相当を作ろうとした.
(accumulation-function <variable>
                       <query pattern>)

;; が, q4.65.scm に見るwheel2重出力問題などを考えるとこの方法はうまくいかない.

