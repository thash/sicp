;; preserving機構を使うと, 翻訳系(sec5.5)は演算子が記号の時は組合せの演算子の評価の前後でenvのsave/restoreを避ける.
;; このような最適化を評価器(sec5.4)にも組み込むことが出来る.
;; envのsave/restoreじゃないけど, 既にsec5.4では被演算子のない組合せを特別に扱って最適化してるよ. あんな感じでよろ

;; (a). 積極制御評価器を拡張し, 演算子が記号である組合せを, 式の別のクラスと認識し,
;;      そういう式の評価にこの事実を利用せよ.

;; symbol?で判別して処理を分けてやる.
;; continueに入れた値はeval-dispatchが終わった後に飛ぶlabel.

ev-application
ev-appl-did-operator
ev-appl-operand-loop
ev-appl-accumulate-arg
ev-appl-last-arg
ev-appl-accum-last-arg

ev-application
ev-operand-symbol
ev-appl-did-operator-not-symbol
ev-appl-did-operator-symbol
ev-appl-did-operator
;; equalではない

;; (b). "多くの特別な場合"を認識させる時点で性能が落ちそうだよね.

