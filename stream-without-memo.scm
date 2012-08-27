;; 読み込むファイルではなく、変数とかで切り替えられないものか...
(define-macro (delay proc)
              `(lambda () ,proc))

(load "./stream-common")
