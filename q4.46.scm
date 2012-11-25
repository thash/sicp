;; 4.1および4.2節の評価器は, 被演算子を評価する順を決めなかった.
;; amb評価器では被演算子を左から右へ評価するのがわかる. 構文解析プログラムが, 被演算子が異なる順で評価されると, 動かなくなる理由を説明せよ.

;; > set! で *unparsed* の先頭から解析をして語句を抜き取ってゆき解析結果を作るようになっているため、構文解析プログラムは amb の評価順序に依存している。
;; > <cite>http://www.serendip.ws/archives/2540</cite>

;; try-againを繰り返して最後まで行くと無限loopに陥る, そうな.

