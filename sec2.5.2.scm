;; 2.5.2. 異なる型のデータの統合
;;
;;   いままで定義した型は完全に独立であったが、
;;   この節では「複素数を通常の数に足す」というような型の境界を越える演算を考える。
(load "./q2.77") ;; includes 2.4.2, 2.4.3, 2.5.1, 3.3.3.

;;   1つの方法: 演算する型の組み合わせ事に別の手続きを設計する

(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; 問題点: 拡張に弱い。型間の演算定義が、型自体の定義の数よりも多くなる。
;;         型Aと型B間の演算はどちらのパッケージに含めるべきか、一貫した基準が持てない。

;; 【強制型変換】

;; 【型の階層構造】



