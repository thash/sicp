;; simple-query と disjoin 手続きがdelay演算を陽に使うのはなぜか.
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
            (stream-append-delayed
              (find-assertions query-pattern frame)
              (delay (apply-rules query-pattern frame))))
    frame-stream))

;; もっと単純に, こんなふうに定義して良いのではないか? 何が悪いのか?
(define (simple-query query-pattern frame-stream)
  (lambda (frame)
    (stream-append (find-assertions query-pattern frame)
                   (apply-rules query-pattern frame)))
  frame-stream)


;; simple-queryを書き換えた上でテスト.
(query-driver-loop)
(assert! (loop 1 2))
(assert! (rule (loop ?x ?y)
               (loop ?y ?x)))

;;; Query input:
(loop 1 ?y)

;;; Query results:
(loop 1 ?y)

;; 無限ループにならないが...

