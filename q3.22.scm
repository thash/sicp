;; "局所状態を持つ手続き"としてキューを実装する。

;; http://oss.timedia.co.jp/show/SICP/ex-3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (front-queue))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                (front-queue)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
              (set-front-ptr! (cdr front-ptr))
              (if (empty-queue?)
                'empty-queue
                (front-queue)))))
    (define (dispatch m)
      (cond ((eq? m 'insert!) insert-queue!)
            ((eq? m 'delete!) (delete-queue!))
            ((eq? m 'print) (front-queue))
            (else
              (error "Undefined operation -- DISPATCH" m))))
    dispatch))

(define (insert-queue! queue item) ((queue 'insert!) item))
(define (delete-queue! queue) (queue 'delete!))
(define (print-queue queue) (queue 'print))


