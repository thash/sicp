(load "./my_defs")
;; memoization/tabulation
;; 手続きに, 以前計算した値を局所的に記憶させる方法。

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; これをmemo化すると
;; 1次元table -- copy from sec3.3.3.scm {{{2
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table) (list '*table*)) ;;; }}}2

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; (memo-fib 3)の計算を解析する環境の図を書け。
;;   => そもそもmemo-fibが動かないのだが..
;;   => 1次元table使ったら大丈夫だった

;; 動きを調べてみた。一回計算した値は（それが過程であれ）記憶しておき、 {{{2
;; lookupで目的の値からカウントダウン、f(n)を計算するときにn-1とn-2を探してあれば使う。
;;
;; gosh> (trace lookup)
;; #<closure (debug:trace-procedure debug:trace-procedure)>
;; gosh> (trace insert!)
;; #<closure (debug:trace-procedure debug:trace-procedure)>
;; gosh> (memo-fib 6)
;; CALL lookup 6 (*table*)
;; RETN lookup #f
;; CALL lookup 5 (*table*)
;; RETN lookup #f
;; CALL lookup 4 (*table*)
;; RETN lookup #f
;; CALL lookup 3 (*table*)
;; RETN lookup #f
;; CALL lookup 2 (*table*)
;; RETN lookup #f
;; CALL lookup 1 (*table*)
;; RETN lookup #f
;; CALL insert! 1 1 (*table*)
;; RETN insert! ok
;; CALL lookup 0 (*table* (1 . 1))
;; RETN lookup #f
;; CALL insert! 0 0 (*table* (1 . 1))
;; RETN insert! ok
;; CALL insert! 2 1 (*table* (0 . 0) (1 . 1))
;; RETN insert! ok
;; CALL lookup 1 (*table* (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 1
;; CALL insert! 3 2 (*table* (2 . 1) (0 . 0) (1 . 1))
;; RETN insert! ok
;; CALL lookup 2 (*table* (3 . 2) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 1
;; CALL insert! 4 3 (*table* (3 . 2) (2 . 1) (0 . 0) (1 . 1))
;; RETN insert! ok
;; CALL lookup 3 (*table* (4 . 3) (3 . 2) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 2
;; CALL insert! 5 5 (*table* (4 . 3) (3 . 2) (2 . 1) (0 . 0) (1 . 1))
;; RETN insert! ok
;; CALL lookup 4 (*table* (...) (...) (3 . 2) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 3
;; CALL insert! 6 8 (*table* (...) (...) (3 . 2) (2 . 1) (0 . 0) (1 . 1))
;; RETN insert! ok
;; 8
;; gosh> (memo-fib 5)
;; CALL lookup 5 (*table* (...) (...) (...) (...) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 5
;; 5
;; gosh> (memo-fib 8)
;; CALL lookup 8 (*table* (...) (...) (...) (...) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup #f
;; CALL lookup 7 (*table* (...) (...) (...) (...) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup #f
;; CALL lookup 6 (*table* (...) (...) (...) (...) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 8
;; CALL lookup 5 (*table* (...) (...) (...) (...) (2 . 1) (0 . 0) (1 . 1))
;; RETN lookup 5
;; CALL insert! 7 13 (*table* (...) (...) (...) (...) (...) (0 . 0) (1 . 1))
;; RETN insert! ok
;; CALL lookup 6 (*table* (...) (...) (...) (...) (...) (...) (...) (1 . 1))
;; RETN lookup 8
;; CALL insert! 8 21 (*table* (...) (...) (...) (...) (...) (...) (...) ...)
;; RETN insert! ok
;; 21
;; }}}2

;; "この方式はmemo-fibを単に(memoize fib)と定義しても動くだろうか。"

