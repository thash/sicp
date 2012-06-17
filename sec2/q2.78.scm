;; Lispの実装は内部的に使う型つきシステムになっている。
;; たとえばnumber?やsymbol?はデータオブジェクトが特定の型であるかどうかを判定する。
;;
;; Q: sec2.4.2.scm で定義した type-tag, contents, attach-tagを修正し、この汎用システムがSchemeの内部型システムの利点が使える様にせよ。
;;    "つまり通常の数は、そのcarが記号scheme-numberである対ではなく、Schemeの数として表現されている点の他は、システムは前の通り動く。"

;; 普通の数でも(add 1 2)的なことを出来るようにしようねという話

;; CALL add 4 3
;;   CALL apply-generic add 4 3
;;     CALL type-tag 4
;; error    RETN type-tag #[unknown]

;; installation
;(load "./my_defs")
;(load "./sec2.4.2") ;; attach-tag, type-tag, contents
;(load "./sec2.4.3") ;; apply-generic
;(load "./sec2.5.1") ;; install-scheme-number-package, add etc
;(load "./sec3.3.3") ;; put/get

;;;; 旧type-tag
;; (define (type-tag datum)
;;   (if (pair? datum)
;;     (car datum)
;;     (error "error" datum)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

;;;; 旧 contents
;; (define (contents datum)
;;   (if (pair? datum)
;;     (cdr datum)
;;     (display "error")))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;;;; 旧 attach-tag
;;(define (attach-tag type-tag contents)
;;  (cons type-tag contents))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

