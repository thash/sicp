;; "セマフォ(semaphore)"は相互排除器の一般化. 相互排除器がaquireできるのは1個だったが、セマフォはn個まで獲得できる。
;; もともとsemaphoreは手旗信号を意味する

;; http://d.hatena.ne.jp/nrvct/20091221/1261402179
;; (a). 相互排除器を使った実装
(define (make-semaphore n)
  (let ((mutex (make-mutex)))
    (define (semaphore m)
      (mutex 'acquire)
      (cond ((eq? m 'acquire)
             (if (= n 0)
               (begin (mutex 'release)
                      (semaphore 'acquire))
               (begin (set! n (- n 1))
                      (mutex 'release))))
            ((eq? m 'release)
             (set! n (+ n 1))
             (mutex 'release))))
    semaphore))

;; (b). test-and-set!を使った実装
(define (make-semaphore n)
  (let ((cell (list #f)))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (if (or (= n 0) (test-and-set! cell))
               (semaphore 'acquire))
             (begin (set! n (- n 1))
                    (clear! cell)))
            ((eq? m 'release)
             (set! n (+ n 1))
             (clear! cell))))
    semaphore))

