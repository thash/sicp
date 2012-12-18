;; stream-flatmapに単純なversionを考えられる.
;; 最初の1個はカラor1個なので(そうなの?)
;; ほげほげしてがっちゃんこできるね, という話

;; これが
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
        (stream-car stream)
        (delay (flatten-stream (stream-cdr stream))))))

;; ここでinterleave-delayedなどを使ってやってるのは無限streamにぶちあたることの回避.

;; じゃあ,シンプルにこう書けるのではという話.
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s))) ;; こっちは名前程度.
(define (simple-flatten stream)
  (stream-map stream-car ;; *1*
              (stream-filter (lambda (s) (not (stream-null? s))) stream)))
;; filterしてnullじゃないstreamだけのstream(ややこしい)にしてしまば,
;; それにmapで射影的にstream-carしても問題ない.
;; mapは作用させて結果を全部返す(length変わらない)けど,
;; filterは文字通りフィルタリングして選別(lengthは<=になる). Rubyのselect.

;; *1* sec3.5.1.scm より stream-car/cdrはこんな定義.
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; この結果は一緒になるらしいが, どうもピンと来ない.
