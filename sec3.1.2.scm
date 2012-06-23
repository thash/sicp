;; 3.1.2. 代入式を取り入れた利点
;;   局所変数を導入した利点の一例として、rand手続きの設計を考える。randは"ランダムな"整数を返す。

;; rand-updateは, x2 = (rand-update x1), x3 = (rand-update x2)...と順々に実行した結果集合が"望みの統計的性質を持つような手続きである。
;; "望みの統計的性質""は例えば "一様分布" とか。
;; "rand-updateを実装する通常のやり方は、a,b,mを適切に選んだ整数とし、mを法としてxをax+bで更新する規則を使うことである。" from p.132 脚注
(define (rand-update x)
  ;... TODO
  }

;; xをfixed value random-initで初期化し、それを逐一rand-updateしていく手続きとして実装できる。
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; random-initに対して毎回rand-updateを呼び出すのではだめなのか？だめ。
;; その理由は、プログラム上は一度呼び出したxの値を覚えておかないといけないから...、すなわち、
;; 毎回毎回、独立名ランダム結果を返してしまうと、その結果集合が"望む統計的性質" -- たとえば一様分布 -- を満たすことが保証できないためである。たぶん。


;; んで、実例としてモンテカルロ法. 大きい集合からランダムにサンプルを選び、実験結果から推定を行う。
;; 例えばランダムに選んだ2つの整数が共通の因子を持たない(= 最大公約数が1となる)確率は6/(pi^2)であることが知られている。これを使って...
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; 注: これはそのまま動かない。問題3.5中で具体的な代替手段を導入する。
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


