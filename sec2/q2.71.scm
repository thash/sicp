(load "./sec2/q2.70.scm")

; N = 5のHuffman木。
; http://www.evernote.com/shard/s11/sh/85a55e3e-9663-4aa1-80aa-24457fcd08a1/b5f530adb83ec3b48202789cf4fb8252
;
; 一般に最高頻度のものが 1 bit,
;       最低頻度のものが N-1 bit?
;
; 2^(n-1)という特徴が生きてない回答なので違いそう。
;; => [20130930] 合ってたっぽいよ

;; 最高頻度のbit数と最低頻度のbit数
;;
;; n=5
;; 最高頻度1bit(e)
;; 最低頻度4bit(a)
;;
;; n=10
;; 最高頻度1bit(j)
;; 最低頻度9bit(a)
;;
;; 一般化
;; 最高頻度   1 bit
;; 最低頻度 n-1 bit

