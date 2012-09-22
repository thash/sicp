;; 時系列での電流/電圧の値を表現するためにstreamを使ってモデル化できる.
;; 抵抗Rと容量Cが直列になったRC回路を考える。
;; 注入電流iに対する回路の電圧応答vの関係.
(load "./sec3.5.3")

(define (RC R C dt)
  (lambda (i-stream v0) (add-streams
                          (integral (scale-stream i-stream (/ 1 C)) v0 dt)
                          (scale-stream i-stream R))))

;; usage
(define RC1 (RC 5 1 0.5))

; (define ones (cons-stream 1 ones))
; (define integers (cons-stream 1 (add-streams ones integers)))
;
; (display-stream-n (RC1 integers 1) 20)
; 6, 11.5, 17.5, 24.0, 31.0, 38.5, 46.5, 55.0, 64.0, 73.5, 83.5, 94.0, 105.0, 116.5, 128.5, 141.0, 154.0, 167.5, 181.5, done


