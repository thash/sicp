;; 3.1.2. 代入式を取り入れた利点

;; "rand-updateを実装する通常のやり方は、a,b,mを適切に選んだ整数とし、mを法としてxをax+bで更新する規則を使うことである。" from p.132 脚注
(define (rand-update x)
  ;... TODO
  }

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


;; モンテカルロ法とは...
;;   大きい集合からランダムにサンプルを選び、実験結果から推定を行う。
;;   ランダムに選んだ2つの整数が共通の因子を持たない(= 最大公約数が1となる)確率は6/(pi^2)であることが知られている。
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    ;; remainingなtrialsが0回であれば手仕舞い, 試行回数に対するテストを通過した割合を返す。
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


;; randではなく、rand-updateを直接使って同じ計算を行う
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (defien (iter trials-remaining trials-passed x)
          (let ((x1 (rand-update x)))
            (let ((x2 (rand-update x1)))
              (cond ((= trials-remaining 0)
                     (/ trials-passed trials))
                    ((= (gcd x1 x2) 1)
                     (iter (- trials-remaining 1)
                           (+ trials-passed 1)
                           x2))
                    (else
                      (iter (- trials-remaining 1)
                            trials-passed
                            x2)))))))


