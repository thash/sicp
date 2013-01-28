;; Alyssa P. Hackerはシミュレータにブレークポイントbreakpoint機能が欲しい.

;; 与えられたラベルのあとn番目の命令の直前にブレークポイントを設定する手続き
(set-breakpoint <machine> <label> <n>)
;; シミュレータがbreakpointに達すると, labelとbreakpointの距離を印字し, 命令の実行を中止する.
;; そこでAlyssaはget-register-contents, set-register-contents!を使うことが出来る.
;; その後,
(proceed-machine <machine>)
;; で実行を続行できなければならない. またbreakpointを削除/前削除する手続きも実装せよ.
(cancel-breakpoint <machine> <label> <n>)
(cancel-all-breakpoint <machine>)

;; 青山さん < これないと困るでしょ
(print-all-breakpoints <machine>)


;; machineのdispatchに加える.
;; 内部アレとしてbreakpoints, を持つ.
;; breakpoints = ((labelname . distance) (labelname . distance)... )



