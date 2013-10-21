; 2.3.4 Huffman 符号化木
;
; Huffman木はアルファベットとその相対頻度を符号化するアルゴリズム.
; https://www.evernote.com/shard/s11/sh/bb4aa287-3a2e-4927-ae04-dbd42376e928/0ecc230e44bdcf18134430e41a82a804
;
;   通信文を0と1で表現する。
;   [固定長符号] 必要な数を表すだけの文字数で01定義。
;     具体的には A: 000, B: 001, C: 010...というように全部3文字。
;     BACADAEFABBAAAGAH, を符号化(= encode?)すると54bit
;   [可変長符号] 必要な数を表すだけの文字数で01定義。
;     具体的には A: 0, B: 100, C: 1010...というように使用頻度(など)に応じて文字数が違う。
;     可変長符号だとどこが1文字の終わりかわかんなくね?
;       => prefix codeを考慮して設計する. 例えばAは0, Bは100なので他の文字は0とか100で始まらない。
;     一般に、通信文中の相対頻度(重み)を利用した符号化をHuffman符号化という。モールス信号もこれ。
;     BACADAEFABBAAAGAH, を符号化すると42bit


;; なぜ出現頻度に応じた重み付けをするの?
;; => 高出現頻度の文字を短いビット数(e=0とか)で表すよう取り決めれば, 通信データ数を節約できる.

; 葉はlist. 先頭要素が"leaf"という記号となるようにする。
; cadrで2個目を取ればsymbol, caddrで3個目を取ればweight
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; 次に木を定義する。木の構成要素は他の木もしくは葉。
; (左 右 "左右のsymbols" "合計weight")
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; gosh> (make-code-tree (make-leaf 'A 4) (make-leaf 'B 1))
; ((leaf A 4) (leaf B 1) (A B) 5)
; caddr => (A B), cadddr => 5
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

;; symbolsとweightは, 対象がleafでもtreeでも使える.
;; このように1種類以上のデータを扱える手続きをgeneric procedureと言う.
;; "ジェネリクス"

;; bits: (0 1 0 1...) の列
(load "./my_defs")
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (trace decode-1)
  (decode-1 bits tree))

;; 左: 0, 右: 1
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else "dame")))

; "重み付き"な要素の集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; 対のリストを葉の順序づけられた集合へ変換する
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)    ; symbol
                             (cadr pair))  ; weight
                  (make-leaf-set (cdr pairs))))))

