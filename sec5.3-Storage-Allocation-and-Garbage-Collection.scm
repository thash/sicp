;;;   5.3 記憶の割当てとごみ集め
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5.4でレジスタ計算機としてScheme評価器(4章のアレ)を実装する.
;; 5.3ではその準備としてリスト構造メモリー(list-structured memory)を用意する.

;; 自動記憶割当て(automatic storage allocation)機能を使い, メモリが無限にあるように錯覚させる.
;; 自動記憶割当ての実現方法の一つが ガベージコレクション garbage collection --
;; すなわちデータオブジェクトが不要になった時, 割り当てられていたメモリを自動的に回収する仕組みである.

;;;     5.3.1 ベクタとしてのメモリ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ベクタという新しいデータ構造を導入する.
;;   (vector-ref  <vector> <n>) はベクタのn番目の要素を返す.
;;   (vector-set! <vector> <n> <value>) はベクタのn番目の要素を, 指定した値に設定する.

;; 計算機メモリでは, このアクセスは,
;; メモリでのベクタ先頭番地を規定するベースアドレス(base address)と
;; ベクタの中の特定の要素の距離を規定する添字(index)を組み合わせるアドレス演算を使って実装できる.

;; ベクタを使ってリストをつくることが出来る.

;; タテが1個の対になってる.
;; それでいてかつthe-cars, the-cdrsというヨコからのアクセスもできる(?)


;;;     5.3.2 無限メモリーの幻想の維持
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 対を再利用するには, 割り当てられた対のどれが不要かを決める方法が必要である.
;; この実現法としてgarbage collectionを取り上げる. これは以下の様な観測に基づく.
;;
;;   "Lisp解釈の任意の時点で, 将来の計算に影響しうるオブジェクトは,
;;    現在, 計算機のレジスタにあるポインタから始めて, carとcdrをなんとか続けて到達可能なものだけである"
;;
;; garbage collectionの方法はいろいろあるが, 本節で取り上げるものはストップアンドコピーstop and copyという.
;;
;;   * メモリを"作業メモリ"と"自由メモリ"に分ける.
;;   * consが対を作るときは作業メモリに割り当てる.
;;   * 作業メモリが満杯になったら"有用(有効)な対"を探してそれを自由メモリの連続する場所へコピーする(GC)
;;   * copyできない = 到達できない対は不要なので, 必要な対はすべて自由メモリにセーブされたことになる.
;;   * 作業メモリと自由メモリの役割を交換する.


;; ストップアンドコピーアルゴリズムをレジスタ計算機の命令の列として規定できる.

begin-garbage-collection
  (assign free (const 0))
  (assign scan (const 0))
  (assign old (reg root))
  (assign relocate-continue (label reassign-root))
  (goto (label relocate-old-result-in-new))
reassign-root
  (assign root (reg new))
  (goto (label gc-loop))

gc-loop
  (test (op =) (reg scan) (reg free))
  (branch (label gc-flip))
  (assign old (op vector-ref) (reg new-cars) (reg scan))
  (assign relocate-continue (label update-car))
  (goto (label relocate-old-result-in-new))

update-car
  (perform
    (op vector-set!) (reg new-cars) (reg scan) (reg new))
  (assign old (op vector-ref) (reg new-cdrs) (reg scan))
  (assign relocate-continue (label update-cdr))
  (goto (label relocate-old-result-in-new))

update-cdr
  (perform
    (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
  (assign scan (op +) (reg scan) (const 1))
  (goto (label gc-loop))

;; サブルーチン. 
relocate-old-result-in-new
  (test (op pointer-to-pair?) (reg old))
  (branch (label pair))
  (assign new (reg old))
  (goto (reg relocate-continue))
pair
  (assign oldcr (op vector-ref) (reg the-cars) (reg old))
  (test (op broken-heart?) (reg oldcr))
  (branch (label already-moved))
  (assign new (reg free)) ; 対の新しい場所
  ;; free pointerを更新
  (assign free (op +) (reg free) (const 1))
  ;; catとcdrを新しいメモリへcopy
  (perform (op vector-set!)
           (reg new-cars) (reg new) (reg oldcr))
  (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
  (perform (op vector-set!)
           (reg new-cdrs) (reg new) (reg oldcr))
  ;; 失恋対を構成
  (perform (op vector-set!)
           (reg the-cars) (reg old) (const broken-heart)) ; 適当なconst?
  (perform
    (op vector-set!) (reg the-cdrs) (reg old) (reg new))
  (goto (reg relocate-continue))
already-moved
  (assign new (op vector-ref) (reg the-cdrs) (reg old))
  (goto (reg relocate-continue))

;; 一番最後でpointerを交換, つまりthe-carsをnew-carsと, the-cdrsをnew-cdrsと交換して,
;; 古いメモリと新しいメモリの役割を交換する.
gc-flip
  (assign temp (reg the-cdrs))
  (assign the-cdrs (reg new-cdrs))
  (assign new-cdrs (reg temp))
  (assign temp (reg the-cars))
  (assign the-cars (reg new-cars))
  (assign new-cars (reg temp))

