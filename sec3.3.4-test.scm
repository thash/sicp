(load "./my_defs")

;(use gauche.test)
;(test-section "logical-or")
;(eqr (logical-or 1 1) => 1)

;; mainの第一引数に実行ファイル名が入ってる

(define (main args)
  (let ((target (car (last-pair (string-split (car args) "/")))))
    (let ((rxobj (rxmatch #/(.*)\./ target)))
      (let ((filename (regexp-replace #/-test/ (rxobj 1) "")))
      (display (string-join (list "./" filename) ""))
      (display (string? (string-join (list "./" filename) "")))
      (load (string-join (list "./" filename) ""))))))



