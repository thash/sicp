;; dequeは、項目が先頭と末尾のどちらでも挿入できる並び。
;; "すべての演算はθ(1)ステップで達成しなければならない。"

;; http://oss.timedia.co.jp/show/SICP/ex-3.23
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        front-ptr))
    (define (rear-queue)
      (if (empty-queue?)
        (error "REAR called with an empty queue")
        rear-ptr))

    (define (front-insert-queue! item)
      (let ((new-pair (cons (cons item '()) front-ptr)))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print-queue))
              (else
                (set-cdr! (car front-ptr) new-pair)
                (set-front-ptr! new-pair)
                (print-queue)))))

    (define (rear-insert-queue! item)
      (let ((new-pair (cons (cons item rear-ptr) '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print-queue))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                (print-queue)))))

    (define (front-delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            ((null? (cdr front-ptr))
             (set-front-ptr! '())
             (set-rear-ptr! '()))
            (else
              (set-front-ptr! (cdr front-ptr))
              (set-cdr! (car front-ptr) '())
              (if (empty-queue?)
                'empty-queue
                (print-queue)))))

    (define (rear-delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            ((null? (cdr front-ptr))
             (set-front-ptr! '())
             (set-rear-ptr! '()))
            (else
              (set-rear-ptr! (cdr (car rear-ptr)))
              (set-cdr! rear-ptr '())
              (if (empty-queue?)
                'empty-queue
                (print-queue)))))

    (define (print-queue)
      (do ((x front-ptr (cdr x)))
        ((null? x) (newline))
        (begin
          (display (caar x))
          (display " "))))

    (define (dispatch m)
      (cond ((eq? m 'front-insert!) front-insert-queue!)
            ((eq? m 'rear-insert!) rear-insert-queue!)
            ((eq? m 'front-delete!) (front-delete-queue!))
            ((eq? m 'rear-delete!) (rear-delete-queue!))
            ((eq? m 'front) (front-queue))
            ((eq? m 'rear) (rear-queue))
            ((eq? m 'empty?) (empty-queue?))
            ((eq? m 'print) (print-queue))
            (else
              (error "Undefined operation -- DISPATCH" m))))
    dispatch))

(define (front-insert-queue! queue item) ((queue 'front-insert!) item))
(define (rear-insert-queue! queue item) ((queue 'rear-insert!) item))
(define (front-delete-queue! queue) (queue 'front-delete!))
(define (rear-delete-queue! queue) (queue 'rear-delete!))
(define (front-queue queue) (queue 'front))
(define (rear-queue queue) (queue 'rear))
(define (empty-queue? queue) (queue 'empty?))
(define (print-queue queue) (queue 'print))


